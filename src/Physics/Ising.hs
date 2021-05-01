{-# LANGUAGE ScopedTypeVariables #-}

module Physics.Ising
  ( -- * Annealing
    Configuration (..),
    SimulationOptions (..),
    -- anneal,
    -- anneal',
    -- simpleAnneal,
    -- groundState,
    simpleGroundState,
    bruteForceSolve,

    -- * Hamiltonian
    Hamiltonian (..),
    computeEnergy,
    computeEnergyChanges,

    -- * Annealing schedule
    linearSchedule,
    exponentialSchedule,

    -- * Sparse matrices
    COO (..),
    CSR (..),
    extractDiagonal,
    fromCOO,
    csrIsSymmetric,
    csrIndex,
    nubBySorted,

    -- * Functions for testing
    loadFromCSV,
    graphErdosRenyi,
    randomHamiltonianM,
    randomHamiltonian,

    -- * Random number generation
    CongruentialState (..),

    -- * Foreign exported functions
  )
where

import Control.Exception (assert)
import Control.Monad.Identity
import Control.Monad.Primitive
import Control.Monad.ST
-- import Control.Monad.State.Strict
import Data.Bits
-- import Data.Coerce
-- import Data.Char (intToDigit)
-- import Data.Foldable (maximum)

import Data.Primitive.ByteArray (MutableByteArray (..), mutableByteArrayContents)
import Data.Primitive.PrimArray
import qualified Data.Primitive.Ptr as P
import Data.Primitive.Types (Prim, sizeOf)
import qualified Data.Text as T
import qualified Data.Text.IO as T
-- import qualified Data.Vector.Generic.Mutable
import Data.Vector.Storable (MVector (..), Storable, Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
-- import Foreign.C.Types

import Foreign.ForeignPtr (ForeignPtr)
import qualified Foreign.ForeignPtr
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr
import Foreign.StablePtr
import GHC.Exts
import GHC.Float
import System.IO.Unsafe (unsafePerformIO)
-- import qualified GHC.Show (show)
-- import Numeric (showIntAtBase)

-- import qualified Language.C.Inline as C
-- import System.Random.MWC
import System.Random.Stateful
import qualified Text.Read
import Prelude hiding (init, toList, trace, words)

modifyPrimArray :: (PrimMonad m, Prim a) => MutablePrimArray (PrimState m) a -> (a -> a) -> Int -> m ()
modifyPrimArray v f i = writePrimArray v i =<< f <$> readPrimArray v i
{-# INLINE modifyPrimArray #-}

modifyVector :: (PrimBase m, Prim a) => MVector (PrimState m) a -> (a -> a) -> Int -> m ()
modifyVector v f i = writeVector v i =<< f <$> readVector v i
{-# INLINE modifyVector #-}

-- | Spin configuration.
--
-- Every spin is represented by a single bit: @bit=0@ means @spin=-1@ and
-- @bit=1@ means @spin=1@.
newtype Configuration = Configuration (PrimArray Word64)
  deriving newtype (Eq, Show)

newtype MutableConfiguration s = MutableConfiguration (MutablePrimArray s Word64)

unsafeFreeze :: PrimMonad m => MutableConfiguration (PrimState m) -> m Configuration
unsafeFreeze (MutableConfiguration v) = Configuration <$> unsafeFreezePrimArray v
{-# INLINE unsafeFreeze #-}

unsafeThaw :: PrimMonad m => Configuration -> m (MutableConfiguration (PrimState m))
unsafeThaw (Configuration v) = MutableConfiguration <$> unsafeThawPrimArray v
{-# INLINE unsafeThaw #-}

thaw :: PrimMonad m => Configuration -> m (MutableConfiguration (PrimState m))
thaw (Configuration v) = unsafeThaw (Configuration v')
  where
    v' = clonePrimArray v 0 (sizeofPrimArray v)

copy :: PrimMonad m => MutableConfiguration (PrimState m) -> MutableConfiguration (PrimState m) -> m ()
copy (MutableConfiguration target) (MutableConfiguration source) =
  assert (sizeofMutablePrimArray target == sizeofMutablePrimArray source) $
    copyMutablePrimArray target 0 source 0 (sizeofMutablePrimArray target)

-- | Get the value of @i@'th spin (either @-1@ or @+1@).
unsafeIndex :: Configuration -> Int -> Int8
unsafeIndex !(Configuration words) !i =
  assert (block < sizeofPrimArray words) $
    (-1) + 2 * fromIntegral (((indexPrimArray words block) `shiftR` rest) `mod` 2)
  where
    block = i `div` 64
    rest = i `mod` 64
{-# INLINE unsafeIndex #-}

-- | Flip @i@'th spin.
unsafeFlip :: PrimMonad m => MutableConfiguration (PrimState m) -> Int -> m ()
unsafeFlip !(MutableConfiguration v) !i =
  assert (block < sizeofMutablePrimArray v) $ do
    x <- readPrimArray v block
    writePrimArray v block (complementBit x rest)
  where
    block = i `div` 64
    rest = i `mod` 64
{-# INLINE unsafeFlip #-}

-- | Mutable simulation state
--
-- It contains current and best spin configurations, histories of current and
-- best energies, and a vector of precomputed ΔE as suggested in [...].
data MutableState s = MutableState
  { currentConfiguration :: !(MutableConfiguration s),
    bestConfiguration :: !(MutableConfiguration s),
    currentEnergyHistory :: !(MVector s Double),
    bestEnergyHistory :: !(MVector s Double),
    energyChanges :: !(MVector s Double)
  }

-- | Annealing options
data SimulationOptions = SimulationOptions
  { optionsHamiltonian :: !Hamiltonian,
    optionsSchedule :: !(Int -> Double),
    optionsNumberSweeps :: !Int
  }

-- | Sparse matrix in Compressed Sparse Row (CSR) format.
data CSR a = CSR
  { csrData :: !(Vector a),
    csrColumnIndices :: {-# UNPACK #-} !(Vector Word32),
    csrRowIndices :: {-# UNPACK #-} !(Vector Word32)
  }
  deriving stock (Eq, Show)

-- | Sparse matrix in Coordinate format.
data COO a = COO
  { cooRowIndices :: {-# UNPACK #-} !(PrimArray Word32),
    cooColumnIndices :: {-# UNPACK #-} !(PrimArray Word32),
    cooData :: {-# UNPACK #-} !(PrimArray a)
  }
  deriving stock (Eq, Show)

data Hamiltonian = Hamiltonian
  { hamiltonianExchange :: {-# UNPACK #-} !(CSR Double),
    hamiltonianOffset :: {-# UNPACK #-} !Double
  }

-- | Return dimension of the Hamiltonian (i.e. number of spins in the system).
dimension :: Hamiltonian -> Int
dimension = numberRows . hamiltonianExchange

linearSchedule ::
  -- | Initial β
  Double ->
  -- | Final β
  Double ->
  -- | Number of sweeps
  Int ->
  -- | Schedule for linearly decreasing β
  (Int -> Double)
linearSchedule β₀ β₁ numberSweeps
  | numberSweeps < 0 = error $ "invalid number of sweeps: " <> show numberSweeps
  | numberSweeps == 0 = \_ -> 0
  | otherwise = \i -> β₀ + c * fromIntegral i
  where
    c = (β₁ - β₀) / fromIntegral (numberSweeps - 1)

exponentialSchedule ::
  -- | Initial β
  Double ->
  -- | Final β
  Double ->
  -- | Number of sweeps
  Int ->
  -- | Schedule for linearly decreasing β
  (Int -> Double)
exponentialSchedule !β₀ !β₁ !numberSweeps
  | numberSweeps < 0 = error $ "invalid number of sweeps: " <> show numberSweeps
  | numberSweeps == 0 = \_ -> 0
  | otherwise = \i -> (β₁ / β₀) ** (fromIntegral i / fromIntegral (numberSweeps - 1))

vectorToPrimArray :: (Prim a, Storable a) => Vector a -> PrimArray a
vectorToPrimArray = primArrayFromList . V.toList

withForeignPtr :: PrimBase m => ForeignPtr a -> (Ptr a -> m b) -> m b
withForeignPtr fp action = unsafeIOToPrim $ Foreign.ForeignPtr.withForeignPtr fp (unsafePrimToIO . action)

withVector :: (PrimBase m, Storable a) => Vector a -> (Ptr a -> m b) -> m b
withVector v = withForeignPtr (fst . V.unsafeToForeignPtr0 $ v)

withMVector :: PrimBase m => MVector (PrimState m) a -> (Ptr a -> m b) -> m b
withMVector (MVector _ fp) = withForeignPtr fp

withMutableState ::
  forall m b.
  PrimBase m =>
  Int ->
  Hamiltonian ->
  Configuration ->
  (MutableState (PrimState m) -> m b) ->
  m (Configuration, Configuration, Vector Double, Vector Double, b)
withMutableState !numberSweeps !hamiltonian !x₀ action = do
  xCurrent <- thaw x₀
  xBest <- thaw x₀
  eCurrent <- MV.unsafeNew (numberSweeps + 1)
  eBest <- MV.unsafeNew (numberSweeps + 1)
  deltaEnergies <- V.unsafeThaw $ computeEnergyChanges hamiltonian x₀
  let e = computeEnergy hamiltonian x₀
  writeVector eCurrent 0 e
  writeVector eBest 0 e
  r <- action $ MutableState xCurrent xBest eCurrent eBest deltaEnergies
  (,,,,) <$> unsafeFreeze xCurrent
    <*> unsafeFreeze xBest
    <*> V.unsafeFreeze eCurrent
    <*> V.unsafeFreeze eBest
    <*> pure r

{-
withMutableState ::
  forall m b.
  PrimBase m =>
  MVector (PrimState m) Double ->
  MVector (PrimState m) Double ->
  Int ->
  Hamiltonian ->
  Configuration ->
  (MutableState (PrimState m) -> m b) ->
  m b
withMutableState currentEnergyHistory' bestEnergyHistory' !numberSweeps !hamiltonian !x action = do
  energyChanges' <- V.unsafeThaw $ computeEnergyChanges hamiltonian x
  _currentConfiguration <- thaw x
  _bestConfiguration <- thaw x
  withMVector currentEnergyHistory' $ \_currentEnergyHistory ->
    withMVector bestEnergyHistory' $ \_bestEnergyHistory ->
      withMVector energyChanges' $ \_energyChanges -> do
        let e = computeEnergy hamiltonian x
        P.writeOffPtr _currentEnergyHistory 0 e
        P.writeOffPtr _bestEnergyHistory 0 e
        let s = MutableState _currentConfiguration _bestConfiguration _currentEnergyHistory _bestEnergyHistory _energyChanges
        action s
-}

-- createMutableState :: PrimMonad m => Int -> Hamiltonian -> Configuration -> m (MutableState (PrimState m))
-- createMutableState !numberSweeps !hamiltonian !x = do
--   _energyChanges <- unsafeThawPrimArray . vectorToPrimArray $ computeEnergyChanges hamiltonian x
--   _currentConfiguration <- thaw x
--   _bestConfiguration <- thaw x
--   _currentEnergyHistory <- MV.new (numberSweeps + 1) -- newAlignedPinnedPrimArray (numberSweeps + 1)
--   _bestEnergyHistory <- MV.new (numberSweeps + 1) -- newAlignedPinnedPrimArray (numberSweeps + 1)
--   let e = computeEnergy hamiltonian x
--   MV.unsafeWrite _currentEnergyHistory 0 e
--   MV.unsafeWrite _bestEnergyHistory 0 e
--   -- writePrimArray _currentEnergyHistory 0 e
--   -- writePrimArray _bestEnergyHistory 0 e
--   return $
--     MutableState
--       _currentConfiguration
--       _bestConfiguration
--       _currentEnergyHistory
--       _bestEnergyHistory
--       _energyChanges

-- unsafeMutablePrimArrayPtr :: forall a s. MutablePrimArray s a -> Ptr a
-- unsafeMutablePrimArrayPtr (MutablePrimArray v) =
--   (castPtr :: Ptr Word8 -> Ptr a) $ mutableByteArrayContents (MutableByteArray v)
-- {-# INLINE unsafeMutablePrimArrayPtr #-}

{-
unsafeSwap :: PrimMonad m => MutablePrimArray (PrimState m) Int -> Int -> Int -> m ()
unsafeSwap v !i !j = do
  a <- readPrimArray v i
  b <- readPrimArray v j
  writePrimArray v i b
  writePrimArray v j a
-- let !p = unsafeMutablePrimArrayPtr v
-- a <- P.readOffPtr p i
-- b <- P.readOffPtr p j
-- P.writeOffPtr p i b
-- P.writeOffPtr p j a
-- return ()
{-# INLINE unsafeSwap #-}
-}

unsafeSwap :: (PrimBase m, Prim a) => MVector (PrimState m) a -> Int -> Int -> m ()
unsafeSwap v !i !j = do
  a <- readVector v i
  b <- readVector v j
  writeVector v i b
  writeVector v j a
{-# INLINE unsafeSwap #-}

shuffleVector :: (PrimBase m, RandomGen g) => MVector (PrimState m) Int -> g -> m g
shuffleVector !v !g₀ = go g₀ n
  where
    !n = MV.length v - 1 -- sizeofMutablePrimArray v - 1
    go !g !j
      | j > 0 = do
        let (i, g') = uniformWord32R' (fromIntegral j) g
        unsafeSwap v j (fromIntegral i)
        go g' (j - 1)
      | otherwise = return g
{-# SCC shuffleVector #-}

generateAcceptanceProbabilities ::
  (RandomGen g, PrimBase m) => Double -> MVector (PrimState m) Double -> g -> m g
generateAcceptanceProbabilities β probabilities g₀ = go g₀ 0
  where
    !n = MV.length probabilities
    go !g !i
      | i < n = do
        let (!u, g') = uniformFloat01 g
        writeVector probabilities i $ (-1 / β) * log (float2Double u)
        go g' (i + 1)
      | otherwise = return g
{-# SCC generateAcceptanceProbabilities #-}

loopM_ :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
loopM_ !begin !end f = go begin
  where
    go !i
      | i < end = f i >> go (i + 1)
      | otherwise = return ()

{-
recomputeEnergyChangesImpl ::
  PrimMonad m =>
  Ptr Double ->
  Ptr Word32 ->
  Ptr Word32 ->
  Int ->
  Configuration ->
  Ptr Double ->
  m ()
recomputeEnergyChangesImpl elements columnIndices rowIndices i bits deltaEnergies = do
  P.writeOffPtr deltaEnergies i =<< (negate <$> P.readOffPtr deltaEnergies i)
  go begin
  where
    begin = fromIntegral $ P.indexOffPtr rowIndices i
    end = fromIntegral $ P.indexOffPtr rowIndices (i + 1)
    pre = fromIntegral $ (-8) * unsafeIndex bits i
    go !k
      | k < end = do
        let j = fromIntegral $ P.indexOffPtr columnIndices k
            coupling = P.indexOffPtr elements k
            σⱼ = fromIntegral $ unsafeIndex bits j
        P.writeOffPtr deltaEnergies j =<< (+ pre * coupling * σⱼ) <$> P.readOffPtr deltaEnergies j
        go (k + 1)
      | otherwise = return ()
-}

recomputeEnergyChanges :: forall m. PrimBase m => Hamiltonian -> MVector (PrimState m) Double -> Int -> Configuration -> m ()
recomputeEnergyChanges hamiltonian deltaEnergies i bits = do
  let (CSR elements columnIndices rowIndices) = hamiltonianExchange hamiltonian
      begin = fromIntegral $ indexVector rowIndices i
      end = fromIntegral $ indexVector rowIndices (i + 1)
      pre = fromIntegral $ (-8) * unsafeIndex bits i
      go !k
        | k < end = do
          let j = fromIntegral $ indexVector columnIndices k
              coupling = indexVector elements k
              σⱼ = fromIntegral $ unsafeIndex bits j
          writeVector deltaEnergies j =<< (+ pre * coupling * σⱼ) <$> readVector deltaEnergies j
          go (k + 1)
        | otherwise = return ()
  writeVector deltaEnergies i =<< (negate <$> readVector deltaEnergies i)
  go begin
-- withVector elements $ \elementsPtr ->
--   withVector columnIndices $ \columnIndicesPtr ->
--     withVector rowIndices $ \rowIndicesPtr ->
--       recomputeEnergyChangesImpl
--         elementsPtr
--         columnIndicesPtr
--         rowIndicesPtr
--         i
--         bits
--         deltaEnergies
{-# SCC recomputeEnergyChanges #-}
{-# INLINEABLE recomputeEnergyChanges #-}

runStep :: PrimBase m => Hamiltonian -> Double -> Int -> Int -> MutableState (PrimState m) -> m ()
runStep !hamiltonian !p !sweep !i !s = do
  accept <- (< p) <$> readVector (energyChanges s) i
  when accept $ do
    let x = currentConfiguration s
    unsafeFlip x i
    recomputeEnergyChanges hamiltonian (energyChanges s) i =<< unsafeFreeze x
    updateCurrent
    maybeUpdateBest x
  where
    updateCurrent = do
      e <- readVector (currentEnergyHistory s) sweep
      δe <- readVector (energyChanges s) i
      writeVector (currentEnergyHistory s) sweep (e - δe)
    maybeUpdateBest x = do
      e <- readVector (currentEnergyHistory s) sweep
      flag <- (e <) <$> readVector (bestEnergyHistory s) sweep
      when flag $ do
        copy (bestConfiguration s) x
        writeVector (bestEnergyHistory s) sweep e
{-# INLINE runStep #-}
{-# SCC runStep #-}

indexVector :: (Storable a, Prim a) => Vector a -> Int -> a
indexVector v i = unsafeInlineIO $ withVector v $ \p -> return $ P.indexOffPtr p i
{-# INLINE indexVector #-}

readVector :: (PrimBase m, Prim a) => MVector (PrimState m) a -> Int -> m a
readVector v i = withMVector v $ \p -> P.readOffPtr p i

writeVector :: (PrimBase m, Prim a) => MVector (PrimState m) a -> Int -> a -> m ()
writeVector v i x = withMVector v $ \p -> P.writeOffPtr p i x

runSweep ::
  (RandomGen g, PrimBase m) =>
  MVector (PrimState m) Int ->
  MVector (PrimState m) Double ->
  Hamiltonian ->
  Double ->
  Int ->
  g ->
  MutableState (PrimState m) ->
  m g
runSweep !order !probabilities !hamiltonian !β !sweep !gen !s = do
  gen' <- generateAcceptanceProbabilities β probabilities =<< shuffleVector order gen
  loopM_ 0 (MV.length order) $ \k -> do
    i <- readVector order k
    p <- readVector probabilities k
    runStep hamiltonian p sweep i s
  return gen'
{-# INLINE runSweep #-}
{-# SCC runSweep #-}

runManySweeps ::
  (RandomGen g, PrimBase m) =>
  MVector (PrimState m) Int ->
  MVector (PrimState m) Double ->
  Int ->
  Hamiltonian ->
  (Int -> Double) ->
  MutableState (PrimState m) ->
  g ->
  m g
runManySweeps order probabilities n hamiltonian schedule s g₀ = go 0 g₀
  where
    go i g
      | i < n = do
        writeVector (currentEnergyHistory s) (i + 1) =<< readVector (currentEnergyHistory s) i
        writeVector (bestEnergyHistory s) (i + 1) =<< readVector (bestEnergyHistory s) i
        go (i + 1) =<< runSweep order probabilities hamiltonian (schedule i) (i + 1) g s
      | otherwise = return g
{-# SCC runManySweeps #-}

runAnnealing ::
  RandomGen g =>
  SimulationOptions ->
  Configuration ->
  g ->
  (Configuration, Configuration, Vector Double, Vector Double, g)
runAnnealing options x₀ g₀ = runST $
  withMutableState (optionsNumberSweeps options) (optionsHamiltonian options) x₀ $ \s -> do
    order <- MV.generate (dimension (optionsHamiltonian options)) id
    probabilities <- MV.unsafeNew (dimension (optionsHamiltonian options))
    runManySweeps
      order
      probabilities
      (optionsNumberSweeps options)
      (optionsHamiltonian options)
      (optionsSchedule options)
      s
      g₀

simpleAnneal ::
  SimulationOptions ->
  Word32 ->
  (Configuration, Configuration, Vector Double, Vector Double)
simpleAnneal options seed =
  let numberSpins = dimension (optionsHamiltonian options)
      (x₀, g) = randomConfiguration' numberSpins $ CongruentialState seed
      (xCurrent, xBest, eCurrent, eBest, _) = runAnnealing options x₀ g
   in (xCurrent, xBest, eCurrent, eBest)

simpleGroundState :: SimulationOptions -> Word32 -> (Configuration, Double)
simpleGroundState options seed =
  let (_, x, _, es) = simpleAnneal options seed
   in (x, V.unsafeIndex es (optionsNumberSweeps options))

{-
runStep ::
  (StatefulGen g m, PrimBase m) =>
  Hamiltonian ->
  Double ->
  Int ->
  Int ->
  g ->
  MutableState (PrimState m) ->
  m ()
runStep !hamiltonian !β !sweep !i !gen !s = do
  !accept <- shouldAccept gen β =<< readPrimArray (energyChanges s) i
  {-# SCC "when1" #-} when accept $ do
    let x = currentConfiguration s
    unsafeFlip x i
    recomputeEnergyChanges hamiltonian (energyChanges s) i =<< {-# SCC "unsafeFreeze" #-} unsafeFreeze x
    -- energy <-
    --   {-# SCC "readCurrent" #-}
    --   (-) <$> readPrimArray (currentEnergyHistory s) sweep
    --     <*> readPrimArray (energyChanges s) i
    updateCurrent
    energy <- readPrimArray (currentEnergyHistory s) sweep
    {-# SCC "writeCurrent" #-} writePrimArray (currentEnergyHistory s) sweep energy
    maybeUpdateBest x energy
    -- !shouldAccept' <- {-# SCC "accept2" #-} (energy <) <$> readPrimArray (bestEnergyHistory s) sweep
    -- {-# SCC "when2" #-} when shouldAccept' $ updateBest x energy
  where
    updateCurrent = do
      e <- readPrimArray (currentEnergyHistory s) sweep
      δe <- readPrimArray (energyChanges s) i
      writePrimArray (currentEnergyHistory s) sweep (e - δe)
    updateBest x e = do
      copy (bestConfiguration s) x
      writePrimArray (bestEnergyHistory s) sweep e
    maybeUpdateBest x e = do
      flag <- (e <) <$> readPrimArray (bestEnergyHistory s) sweep
      when flag $ updateBest x e
{-# INLINE runStep #-}
{-# SCC runStep #-}
-}

{-
runSweep ::
  (StatefulGen g m, PrimBase m) =>
  MutablePrimArray (PrimState m) Int ->
  Hamiltonian ->
  Double ->
  Int ->
  g ->
  MutableState (PrimState m) ->
  m ()
runSweep !order !hamiltonian !β !sweep !gen !s = do
  -- let n = numberRows (hamiltonianExchange hamiltonian)
  shuffleVector order gen
  -- order <- randomPermutation n gen
  unsafeFreezePrimArray order >>= \order' -> flip traversePrimArray_ order' $ \i ->
    runStep hamiltonian β sweep i gen s
{-# SCC runSweep #-}
-}

initialOrder :: Int -> PrimArray Int
initialOrder n = runST $ do
  marr <- newAlignedPinnedPrimArray n
  let go !i =
        if i < n
          then do
            writePrimArray marr i i
            go (i + 1)
          else return ()
  go 0
  unsafeFreezePrimArray marr

{-
anneal' ::
  (StatefulGen g m, PrimBase m) =>
  SimulationOptions ->
  Configuration ->
  g ->
  m (Configuration, Configuration, PrimArray Double, PrimArray Double)
anneal' options init gen = do
  s <- createMutableState (optionsNumberSweeps options) (optionsHamiltonian options) init
  order <- unsafeThawPrimArray $ initialOrder (dimension (optionsHamiltonian options))
  -- probabilities <- newAlignedPinnedPrimArray (dimension (optionsHamiltonian options))
  loopM_ 0 (optionsNumberSweeps options) $ \i -> do
    runSweep order (optionsHamiltonian options) (optionsSchedule options i) i gen s
    writePrimArray (currentEnergyHistory s) (i + 1) =<< readPrimArray (currentEnergyHistory s) i
    writePrimArray (bestEnergyHistory s) (i + 1) =<< readPrimArray (bestEnergyHistory s) i
  xCurrent <- unsafeFreeze $ currentConfiguration s
  xBest <- unsafeFreeze $ bestConfiguration s
  eCurrent <- unsafeFreezePrimArray $ currentEnergyHistory s
  eBest <- unsafeFreezePrimArray $ bestEnergyHistory s
  return (xCurrent, xBest, eCurrent, eBest)
-}

randomConfiguration' :: RandomGen g => Int -> g -> (Configuration, g)
randomConfiguration' n g₀ = runST $ do
  buffer <- newPrimArray $ (n + 63) `div` 64
  let goMain !i !g
        | i < blocks = do
          let (w, g') = uniform g
          writePrimArray buffer i w
          goMain (i + 1) g'
        | otherwise = goFinal i g
      goFinal !i !g =
        if (rest > 0)
          then do
            let (w, g') = uniformR (0, 2 ^ rest - 1) g
            writePrimArray buffer i w
            return g'
          else return g
  g <- goMain 0 g₀
  x <- unsafeFreeze $ MutableConfiguration buffer
  return (x, g)
  where
    blocks = n `div` 64
    rest = n `mod` 64

{-
randomConfiguration :: StatefulGen g m => Int -> g -> m Configuration
randomConfiguration n gen = do
  let blocks = n `div` 64
      rest = n `mod` 64
  completeWords <- replicateM blocks (uniformM gen)
  if rest > 0
    then do
      lastWord <- uniformRM (0, 2 ^ rest - 1) gen
      return . Configuration . fromList $ completeWords ++ [lastWord]
    else return . Configuration . fromList $ completeWords
-}

{-
anneal ::
  (StatefulGen g m, PrimBase m) =>
  SimulationOptions ->
  g ->
  m (Configuration, Configuration, PrimArray Double, PrimArray Double)
anneal options gen
  | numberSpins == 0 = error $ "invalid number of spins: " <> show numberSpins
  | otherwise = do
    init <- randomConfiguration numberSpins gen
    anneal' options init gen
  where
    numberSpins = dimension (optionsHamiltonian options)
-}

{-
groundState ::
  (StatefulGen g m, PrimBase m) =>
  SimulationOptions ->
  g ->
  m (Configuration, Double)
groundState options gen = do
  (_, x, _, es) <- anneal options gen
  return (x, indexPrimArray es (optionsNumberSweeps options - 1))
-}

bruteForceSolve :: Hamiltonian -> (Configuration, Double)
bruteForceSolve hamiltonian
  | n == 0 = error $ "invalid number of spins: " <> show n
  | n >= 64 = error "it is unfeasible to iterate over more than 2⁶³ spin configurations"
  | otherwise = runST $ do
    let x₀ = Configuration (fromList [0])
        e₀ = computeEnergy hamiltonian x₀
    buffer <- unsafeThaw x₀
    let go !xBest !eBest x
          | x < end = do
            let (MutableConfiguration v) = buffer
            writePrimArray v 0 x
            e <- computeEnergy hamiltonian <$> unsafeFreeze buffer
            if e < eBest
              then go x e (x + 1)
              else go xBest eBest (x + 1)
          | otherwise = return (Configuration $ fromList [xBest], eBest)
    go 0 e₀ 1
  where
    !n = dimension hamiltonian
    !end = 2 ^ n

----------------------------------------------------------------------------------------------------
-- Hamiltonian
----------------------------------------------------------------------------------------------------

-- | Computes one element of a matrix-vector product ∑ⱼMᵢⱼvⱼ
matrixVectorProductElement :: (Storable a, Num a) => CSR a -> Configuration -> Int -> a
matrixVectorProductElement !(CSR elements columnIndices rowIndices) !v !i =
  assert (i < V.length rowIndices - 1) $ go 0 begin end
  where
    begin = fromIntegral $ V.unsafeIndex rowIndices i
    end = fromIntegral $ V.unsafeIndex rowIndices (i + 1)
    go !acc !k !n
      | k < n =
        let !j = fromIntegral $ V.unsafeIndex columnIndices k
            !coupling = V.unsafeIndex elements k
            !σⱼ = fromIntegral $ unsafeIndex v j
         in go (acc + coupling * σⱼ) (k + 1) n
      | otherwise = acc

-- | Compute energy of a classical spin configuration
computeEnergy :: Hamiltonian -> Configuration -> Double
computeEnergy !hamiltonian !configuration =
  hamiltonianOffset hamiltonian + goColumn 0 0 (numberRows matrix)
  where
    !matrix = hamiltonianExchange hamiltonian
    goColumn !acc !i !n
      | i < n =
        let !σᵢ = fromIntegral $ unsafeIndex configuration i
            !acc' = acc + σᵢ * matrixVectorProductElement matrix configuration i
         in goColumn acc' (i + 1) n
      | otherwise = acc

-- | Compute 'energyChangeUponFlip' for every spin.
computeEnergyChanges :: Hamiltonian -> Configuration -> Vector Double
computeEnergyChanges hamiltonian configuration = V.generate n energyChangeUponFlip
  where
    n = numberRows $ hamiltonianExchange hamiltonian
    σ i = fromIntegral $ unsafeIndex configuration i
    energyChangeUponFlip i =
      -4 * σ i * matrixVectorProductElement (hamiltonianExchange hamiltonian) configuration i

----------------------------------------------------------------------------------------------------
-- Sparse matrices
----------------------------------------------------------------------------------------------------

instance Prim a => IsList (COO a) where
  type Item (COO a) = (Word32, Word32, a)
  fromList coo =
    let noValue f (a₁, a₂, _) (b₁, b₂, _) = f (a₁, a₂) (b₁, b₂)
        coo' = nubBySorted (noValue (==)) $ sortBy (noValue compare) coo
        rowIndices = fromList $ (\(i, _, _) -> i) <$> coo'
        columnIndices = fromList $ (\(_, j, _) -> j) <$> coo'
        elements = fromList $ (\(_, _, e) -> e) <$> coo'
     in COO rowIndices columnIndices elements
  toList = error "toList is not implemented for COO"

nubBySorted :: (a -> a -> Bool) -> [a] -> [a]
nubBySorted eq list = foldr acc [] list
  where
    acc x [] = [x]
    acc x ys@(y : _) = if eq x y then ys else x : ys

-- | Return number of rows in the matrix
numberRows :: CSR a -> Int
numberRows csr = V.length (csrRowIndices csr) - 1

binarySearch :: Ord a => (Int -> a) -> Int -> Int -> a -> Maybe Int
binarySearch atIndex lower₀ upper₀ !z = go lower₀ upper₀
  where
    go !lower !upper
      | lower >= upper = Nothing
      | otherwise =
        let !i = lower + (upper - lower) `div` 2
            !x = atIndex i
         in case compare x z of
              LT -> go i upper
              GT -> go lower i
              EQ -> Just i

csrIndex :: Storable a => CSR a -> Int -> Int -> Maybe a
csrIndex csr i j
  | i < numberRows csr =
    let begin = fromIntegral $ V.unsafeIndex (csrRowIndices csr) i
        end = fromIntegral $ V.unsafeIndex (csrRowIndices csr) (i + 1)
        atIndex k =
          assert (k < V.length (csrColumnIndices csr)) $
            V.unsafeIndex (csrColumnIndices csr) k
     in V.unsafeIndex (csrData csr) <$> binarySearch atIndex begin end (fromIntegral j)
  | otherwise = error $ "index out of bounds: " <> show i

csrFoldlM :: (Storable a, Monad m) => (b -> Int -> Int -> a -> m b) -> b -> CSR a -> m b
csrFoldlM f init matrix@(CSR elements columnIndices rowIndices) = foldlM foldlRow init [0 .. numberRows matrix - 1]
  where
    foldlRow z i =
      let begin = fromIntegral $ V.unsafeIndex rowIndices i
          end = fromIntegral $ V.unsafeIndex rowIndices (i + 1)
          combine z' k =
            let j = fromIntegral $ V.unsafeIndex columnIndices k
                e = V.unsafeIndex elements k
             in f z' i j e
       in foldlM combine z [begin .. end - 1]

csrIsSymmetricBy :: Storable a => (a -> a -> Bool) -> CSR a -> Bool
csrIsSymmetricBy equal csr = runIdentity $ csrFoldlM combine True csr
  where
    combine False _ _ _ = return False
    combine True i j e
      | i >= j = return True
      | otherwise = return $ maybe False (equal e) (csrIndex csr i j)

csrIsSymmetric :: (Storable a, Eq a) => CSR a -> Bool
csrIsSymmetric = csrIsSymmetricBy (==)
{-# INLINE csrIsSymmetric #-}
{-# SCC csrIsSymmetric #-}

cooShape :: COO a -> (Int, Int)
cooShape (COO rowIndices columnIndices _) = (dim rowIndices, dim columnIndices)
  where
    dim xs
      | sizeofPrimArray xs == 0 = 0
      | otherwise = fromIntegral $ 1 + foldlPrimArray' max 0 xs
{-# INLINE cooShape #-}

cooDim :: COO a -> Int
cooDim coo
  | n == m = n
  | otherwise = error $ "matrix is not square: " <> show n <> " != " <> show m
  where
    (n, m) = cooShape coo
{-# INLINE cooDim #-}

extractDiagonal :: (Num a, Prim a) => COO a -> (COO a, a)
extractDiagonal (COO rowIndices columnIndices elements) = runST $ do
  let n = sizeofPrimArray rowIndices
  rowIndices' <- newAlignedPinnedPrimArray n
  columnIndices' <- newAlignedPinnedPrimArray n
  elements' <- newAlignedPinnedPrimArray n
  let go !acc !offset !i
        | i < n =
          if indexPrimArray rowIndices i == indexPrimArray columnIndices i
            then go (acc + indexPrimArray elements i) offset (i + 1)
            else do
              writePrimArray rowIndices' offset $ indexPrimArray rowIndices i
              writePrimArray columnIndices' offset $ indexPrimArray columnIndices i
              writePrimArray elements' offset $ indexPrimArray elements i
              go acc (offset + 1) (i + 1)
        | otherwise = return (acc, offset)
  (trace, n') <- go 0 0 0
  when (n' < n) $ do
    shrinkMutablePrimArray rowIndices' n'
    shrinkMutablePrimArray columnIndices' n'
    shrinkMutablePrimArray elements' n'
  coo' <-
    COO <$> unsafeFreezePrimArray rowIndices'
      <*> unsafeFreezePrimArray columnIndices'
      <*> unsafeFreezePrimArray elements'
  return (coo', trace)
{-# SCC extractDiagonal #-}

fromCOO :: (Prim a, Storable a) => COO a -> CSR a
fromCOO coo = CSR elements columnIndices rowIndices
  where
    !n = cooDim coo
    !elements = fromList . toList $ cooData coo
    !columnIndices = fromList . toList $ cooColumnIndices coo
    !rowIndices = fromList . toList $
      runST $ do
        rs <- unsafeThawPrimArray $ replicatePrimArray (n + 1) 0
        flip traversePrimArray_ (cooRowIndices coo) $ \i ->
          modifyPrimArray rs (+ 1) (fromIntegral i + 1)
        loopM_ 0 n $ \i -> do
          r <- readPrimArray rs i
          modifyPrimArray rs (+ r) (i + 1)
        unsafeFreezePrimArray rs
{-# SCC fromCOO #-}

----------------------------------------------------------------------------------------------------
-- Functions for testing
----------------------------------------------------------------------------------------------------

skipUniformly :: StatefulGen g m => Double -> g -> [a] -> m [a]
skipUniformly p gen list
  | p < 0 || p > 1 = error $ "invalid probability p: " <> show p
  -- Special case p = 1 because uniformRM returns numbers in [0, 1] rather than [0, 1)
  | p == 1 = return list
  | otherwise = do
    us <- replicateM (length list) $ uniformRM (0, 1) gen
    return . map fst . filter ((< p) . snd) . zip list $ us

graphErdosRenyi :: StatefulGen g m => Int -> Double -> g -> m [(Int, Int)]
graphErdosRenyi n p gen
  | n < 0 = error $ "invalid number of nodes n: " <> show n
  | n == 0 = return []
  | otherwise = do
    let edges = [(i, j) | i <- [0 .. (n - 2)], j <- [(i + 1) .. (n - 1)]]
    skipUniformly p gen edges

randomHamiltonianM :: StatefulGen g m => Int -> Double -> g -> m Hamiltonian
randomHamiltonianM n p gen = do
  graph <- graphErdosRenyi n p gen
  couplings <- replicateM (length graph) (uniformRM (-1, 1) gen)
  let coo = zipWith (\(i, j) c -> (fromIntegral i, fromIntegral j, c)) graph couplings
      csr = fromCOO . fromList $ coo ++ map (\(i, j, c) -> (j, i, c)) coo
  return $ Hamiltonian csr 0

randomHamiltonian :: RandomGen g => Int -> Double -> g -> (Hamiltonian, g)
randomHamiltonian n p g = runStateGen g (randomHamiltonianM n p)

loadFromCSV :: String -> IO Hamiltonian
loadFromCSV filename = do
  contents <- lines <$> T.readFile filename
  let parse [i, j, c] = (Text.Read.read i, Text.Read.read j, Text.Read.read c)
      parse parts = error $ "Parsing " <> show filename <> " failed: " <> show parts
      (coo, diagonal) = extractDiagonal . fromList $ parse . map toString . T.splitOn "," <$> contents
  return $ Hamiltonian (fromCOO coo) diagonal
{-# SCC loadFromCSV #-}

----------------------------------------------------------------------------------------------------
-- Random number generation
----------------------------------------------------------------------------------------------------

newtype CongruentialState = CongruentialState Word32

instance RandomGen CongruentialState where
  split = error "CongruentialState is not splittable"
  genWord32 (CongruentialState s) =
    let (a :: Word64) = 0xadb4a92d
        (c :: Word64) = 1
        !s' = fromIntegral $ a * fromIntegral s + c
     in (s', CongruentialState s')
  {-# INLINE genWord32 #-}
  {-# SCC genWord32 #-}
  genWord64 g =
    let (w₁, g₁) = genWord32 g
        (w₂, g₂) = genWord32 g₁
     in ((fromIntegral w₁ `shiftL` 32) .|. fromIntegral w₂, g₂)
  {-# INLINE genWord64 #-}
  {-# SCC genWord64 #-}

uniformWord32R' :: forall g. RandomGen g => Word32 -> g -> (Word32, g)
uniformWord32R' !r !g₀ = go g₀
  where
    -- t :: Word32
    -- t = (- r) `mod` r -- Calculates 2^32 `mod` r!!!
    go :: g -> (Word32, g)
    go g =
      let (w, g') = genWord32 g
          (m :: Word64) = fromIntegral r * fromIntegral w
       in (fromIntegral (m `shiftR` 32), g')

wordToFloat :: Word32 -> Float
wordToFloat x = (fromIntegral i * m_inv_32) + 0.5 + m_inv_33
  where
    m_inv_33 = 1.16415321826934814453125e-10
    m_inv_32 = 2.3283064365386962890625e-10
    i = fromIntegral x :: Int32
{-# INLINE wordToFloat #-}

uniformFloat01 :: RandomGen g => g -> (Float, g)
uniformFloat01 !g =
  let (w, g') = genWord32 g
   in (wordToFloat w, g')
{-# INLINE uniformFloat01 #-}
{-# SCC uniformFloat01 #-}

----------------------------------------------------------------------------------------------------
-- Foreign exported functions
----------------------------------------------------------------------------------------------------

primArrayFromPtr :: forall a. Prim a => Int -> Ptr a -> IO (PrimArray a)
primArrayFromPtr n p = do
  let elementSize = let x = x :: a in sizeOf x
  v <- newAlignedPinnedPrimArray n
  copyBytes (mutablePrimArrayContents v) p (n * elementSize)
  unsafeFreezePrimArray v

create_hamiltonian ::
  Word32 ->
  Ptr Word32 ->
  Ptr Word32 ->
  Ptr Double ->
  IO (StablePtr Hamiltonian)
create_hamiltonian n rowIndicesPtr columnIndicesPtr dataPtr = do
  let count = fromIntegral n
  exchange <-
    COO <$> primArrayFromPtr count rowIndicesPtr
      <*> primArrayFromPtr count columnIndicesPtr
      <*> primArrayFromPtr count dataPtr
  let (exchange', trace) = extractDiagonal exchange
      hamiltonian = Hamiltonian (fromCOO exchange') trace
  newStablePtr hamiltonian

destroy_hamiltonian :: StablePtr Hamiltonian -> IO ()
destroy_hamiltonian = freeStablePtr

foreign export ccall create_hamiltonian :: Word32 -> Ptr Word32 -> Ptr Word32 -> Ptr Double -> IO (StablePtr Hamiltonian)

foreign export ccall destroy_hamiltonian :: StablePtr Hamiltonian -> IO ()
