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
    mkHamiltonian,
    computeEnergy,
    computeEnergyChanges,
    computeOverlap,

    -- * Annealing schedule
    linearSchedule,
    exponentialSchedule,
    estimateBetas,

    -- * Sparse matrices
    COO (..),
    CSR (..),
    mkCOO,
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

-- import Data.Primitive.ByteArray (MutableByteArray (..), mutableByteArrayContents)
import Data.Primitive.PrimArray
import qualified Data.Primitive.Ptr as P
import Data.Primitive.Types (Prim, sizeOf)
import qualified Data.Text as T
import qualified Data.Text.IO as T
-- import qualified Data.Vector.Generic.Mutable

import qualified Data.Vector.Algorithms.Intro as MV
import Data.Vector.Storable (MVector (..), Storable, Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
-- import Foreign.C.Types

import Foreign.ForeignPtr (ForeignPtr, newForeignPtr_)
import qualified Foreign.ForeignPtr
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr
import Foreign.StablePtr
import GHC.Exts
import GHC.Float
-- import System.IO.Unsafe (unsafePerformIO)
-- import qualified GHC.Show (show)
-- import Numeric (showIntAtBase)

-- import qualified Language.C.Inline as C
-- import System.Random.MWC
import System.Random.Stateful
import qualified Text.Read
import Prelude hiding (first, init, second, toList, trace, words)

withForeignPtr :: PrimBase m => ForeignPtr a -> (Ptr a -> m b) -> m b
withForeignPtr fp action = unsafeIOToPrim $ Foreign.ForeignPtr.withForeignPtr fp (unsafePrimToIO . action)

withVector :: (PrimBase m, Storable a) => Vector a -> (Ptr a -> m b) -> m b
withVector v = withForeignPtr (fst . V.unsafeToForeignPtr0 $ v)

withMVector :: PrimBase m => MVector (PrimState m) a -> (Ptr a -> m b) -> m b
withMVector (MVector _ fp) = withForeignPtr fp

indexVector :: (Storable a, Prim a) => Vector a -> Int -> a
indexVector v i = unsafeInlineIO $ withVector v $ \p -> return $ P.indexOffPtr p i
{-# INLINE indexVector #-}

infixl 9 !

(!) :: (Storable a, Prim a) => Vector a -> Int -> a
(!) = indexVector
{-# INLINE (!) #-}

readVector :: (PrimBase m, Prim a) => MVector (PrimState m) a -> Int -> m a
readVector v i = withMVector v $ \p -> P.readOffPtr p i

writeVector :: (PrimBase m, Prim a) => MVector (PrimState m) a -> Int -> a -> m ()
writeVector v i x = withMVector v $ \p -> P.writeOffPtr p i x

modifyVector :: (PrimBase m, Prim a) => MVector (PrimState m) a -> (a -> a) -> Int -> m ()
modifyVector v f i = writeVector v i =<< f <$> readVector v i
{-# INLINE modifyVector #-}

loopM_ :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
loopM_ !begin !end f = go begin
  where
    go !i
      | i < end = f i >> go (i + 1)
      | otherwise = return ()

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
  { cooRowIndices :: {-# UNPACK #-} !(Vector Word32),
    cooColumnIndices :: {-# UNPACK #-} !(Vector Word32),
    cooData :: {-# UNPACK #-} !(Vector a)
  }
  deriving stock (Eq, Show)

data Hamiltonian = Hamiltonian
  { hamiltonianExchange :: {-# UNPACK #-} !(CSR Double),
    hamiltonianField :: {-# UNPACK #-} !(Vector Double),
    hamiltonianOffset :: {-# UNPACK #-} !Double
  }
  deriving stock (Show)

-- | Return dimension of the Hamiltonian (i.e. number of spins in the system).
dimension :: Hamiltonian -> Int
dimension = V.length . hamiltonianField

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
  | otherwise = \i -> β₀ * (β₁ / β₀) ** (fromIntegral i / fromIntegral (numberSweeps - 1))

estimateBetas :: Hamiltonian -> (Double, Double)
estimateBetas hamiltonian = (log 2 / maxDeltaEnergy, log 100 / minDeltaEnergy)
  where
    (minDeltaEnergy, maxDeltaEnergy) = energyChangeBounds hamiltonian

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

computeOverlap :: Vector Double -> Configuration -> Double
computeOverlap exact predicted@(Configuration array)
  | V.length exact <= 64 * sizeofPrimArray array = let (s, n) = V.ifoldl' combine (0, 0) exact in s / n
  | otherwise = error $ "lengths of 'exact' and 'predicted' do not match"
  where
    combine (s, n) i x' =
      let y = fromIntegral $ unsafeIndex predicted i
          x = signum x'
          w = abs x' * abs x'
       in (s + w * x * y, n + w)

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

dotProduct :: (Storable a, Prim a, Num a) => Vector a -> Configuration -> a
dotProduct !field !v = go 0 0
  where
    n = V.length field
    go acc i
      | i >= n = acc
      | otherwise =
        let t = fromIntegral (unsafeIndex v i) * indexVector field i
         in go (acc + t) (i + 1)

-- | Compute energy of a classical spin configuration
computeEnergy :: Hamiltonian -> Configuration -> Double
computeEnergy !hamiltonian !configuration =
  hamiltonianOffset hamiltonian
    + dotProduct (hamiltonianField hamiltonian) configuration
    + goColumn 0 0 (numberRows matrix)
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
    n = dimension hamiltonian
    σ i = fromIntegral $ unsafeIndex configuration i
    energyChangeUponFlip i =
      - σ i
        * ( 4 * matrixVectorProductElement (hamiltonianExchange hamiltonian) configuration i
              + 2 * indexVector (hamiltonianField hamiltonian) i
          )

minEnergyChange :: Hamiltonian -> Int -> Double
minEnergyChange hamiltonian i = 2 * max first second
  where
    first = abs $ indexVector (hamiltonianField hamiltonian) i
    second = csrFoldRow (\(!z) i' j e -> if i' == j then z else min z (abs e)) (1 / 0) (hamiltonianExchange hamiltonian) i

maxEnergyChange :: Hamiltonian -> Int -> Double
maxEnergyChange hamiltonian i = 2 * (first + second)
  where
    first = abs $ indexVector (hamiltonianField hamiltonian) i
    second = csrFoldRow (\(!z) i' j e -> if i' == j then z else z + abs e) 0 (hamiltonianExchange hamiltonian) i

energyChangeBounds :: Hamiltonian -> (Double, Double)
energyChangeBounds hamiltonian = (max lower (2.220446049250313e-16 * upper), upper)
  where
    n = dimension hamiltonian
    lower = runIdentity $ fold1 0 (< n) (+ 1) (\z i -> return $ min z (minEnergyChange hamiltonian i)) (1 / 0)
    upper = runIdentity $ fold1 0 (< n) (+ 1) (\z i -> return $ max z (maxEnergyChange hamiltonian i)) 0

----------------------------------------------------------------------------------------------------
-- Sparse matrices
----------------------------------------------------------------------------------------------------

instance (Storable a, Prim a) => IsList (COO a) where
  type Item (COO a) = (Word32, Word32, a)
  fromList coo =
    let rowIndices = fromList $ (\(i, _, _) -> i) <$> coo
        columnIndices = fromList $ (\(_, j, _) -> j) <$> coo
        elements = fromList $ (\(_, _, e) -> e) <$> coo
     in mkCOO rowIndices columnIndices elements
  toList = error "toList is not implemented for COO"

nubBySorted :: (a -> a -> Bool) -> [a] -> [a]
nubBySorted eq list = foldr acc [] list
  where
    acc x [] = [x]
    acc x ys@(y : _) = if eq x y then ys else x : ys

sortedIndices :: Vector Word32 -> Vector Word32 -> Vector Int
sortedIndices rowIndices columnIndices = runST $ do
  let cmp x y = compare (rowIndices ! x, columnIndices ! x) (rowIndices ! y, columnIndices ! y)
  order <- MV.generate (V.length rowIndices) id
  MV.sortBy cmp order
  V.unsafeFreeze order

permute :: (Storable a, Prim a) => Vector a -> Vector Int -> Vector a
permute xs order = V.map (xs !) order

sortMatrix :: (Storable a, Prim a) => COO a -> COO a
sortMatrix (COO rowIndices columnIndices elements) =
  let order = sortedIndices rowIndices columnIndices
   in COO
        (permute rowIndices order)
        (permute columnIndices order)
        (permute elements order)

hasDuplicates :: COO a -> Bool
hasDuplicates (COO rowIndices columnIndices _) = go 1
  where
    n = V.length rowIndices
    go !i
      | i < n =
        if rowIndices ! (i - 1) == rowIndices ! i
          && columnIndices ! (i - 1) == columnIndices ! i
          then True
          else go (i + 1)
      | otherwise = False

checkDuplicates :: COO a -> COO a
checkDuplicates matrix
  | hasDuplicates matrix = error "COO matrix contains duplicate matrix elements"
  | otherwise = matrix

extractDiagonal :: (Num a, Storable a) => COO a -> (COO a, a)
extractDiagonal (COO rowIndices columnIndices elements) =
  (COO rowIndices' columnIndices' elements', diagonal)
  where
    isDiagonal !i = rowIndices ! i == columnIndices ! i
    onlyOffDiagonal :: Storable a => Vector a -> Vector a
    onlyOffDiagonal = V.ifilter (\i _ -> not $ isDiagonal i)
    rowIndices' = onlyOffDiagonal rowIndices
    columnIndices' = onlyOffDiagonal columnIndices
    elements' = onlyOffDiagonal elements
    diagonal = V.sum $ V.ifilter (\i _ -> isDiagonal i) elements
{-# SCC extractDiagonal #-}

mkCOO :: (Storable a, Prim a) => Vector Word32 -> Vector Word32 -> Vector a -> COO a
mkCOO rowIndices columnIndices elements
  | V.length rowIndices /= V.length columnIndices
      || V.length rowIndices /= V.length elements =
    error $
      "lengths of rowIndices, columnIndices, and elements do not match: "
        <> show (V.length rowIndices)
        <> " vs. "
        <> show (V.length columnIndices)
        <> " vs. "
        <> show (V.length elements)
  | otherwise = checkDuplicates . sortMatrix $ COO rowIndices columnIndices elements
{-# SCC mkCOO #-}

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

fold1 :: Monad m => a -> (a -> Bool) -> (a -> a) -> (b -> a -> m b) -> b -> m b
fold1 start cond inc combine init = go start init
  where
    go !x !acc
      | cond x = acc `combine` x >>= go (inc x)
      | otherwise = return acc
{-# INLINE fold1 #-}

-- loop1 :: Monad m => a -> (a -> Bool) -> (a -> a) -> (a -> m ()) -> m ()
-- loop1 start cond inc f = go start
--   where
--     go !x
--       | cond x = f x >> go (inc x)
--       | otherwise = return ()
-- {-# INLINE loop1 #-}

csrFoldRowM :: forall a b m. (Storable a, Prim a, Monad m) => (b -> Int -> Int -> a -> m b) -> b -> CSR a -> Int -> m b
csrFoldRowM f init (CSR elements columnIndices rowIndices) i = fold1 begin (< end) (+ 1) combine init
  where
    begin :: Int
    !begin = fromIntegral $ indexVector rowIndices i
    end :: Int
    !end = fromIntegral $ indexVector rowIndices (i + 1)
    combine :: b -> Int -> m b
    combine !z !k =
      let j = fromIntegral $ indexVector columnIndices k
          e = indexVector elements k
       in f z i j e

csrFoldRow :: (Storable a, Prim a) => (b -> Int -> Int -> a -> b) -> b -> CSR a -> Int -> b
csrFoldRow f z₀ matrix i₀ = runIdentity $ csrFoldRowM (\z i j e -> return $ f z i j e) z₀ matrix i₀

csrFoldM :: (Storable a, Prim a, Monad m) => (b -> Int -> Int -> a -> m b) -> b -> CSR a -> m b
csrFoldM f init matrix = fold1 0 (< numberRows matrix) (+ 1) (\z i -> csrFoldRowM f z matrix i) init

-- csrFoldlM :: (Storable a, Monad m) => (b -> Int -> Int -> a -> m b) -> b -> CSR a -> m b
-- csrFoldlM f init matrix@(CSR elements columnIndices rowIndices) = foldlM foldlRow init [0 .. numberRows matrix - 1]
--   where
--     foldlRow z i =
--       let begin = fromIntegral $ V.unsafeIndex rowIndices i
--           end = fromIntegral $ V.unsafeIndex rowIndices (i + 1)
--           combine z' k =
--             let j = fromIntegral $ V.unsafeIndex columnIndices k
--                 e = V.unsafeIndex elements k
--              in f z' i j e
--        in foldlM combine z [begin .. end - 1]

csrIsSymmetricBy :: (Storable a, Prim a) => (a -> a -> Bool) -> CSR a -> Bool
csrIsSymmetricBy equal csr = runIdentity $ csrFoldM combine True csr
  where
    combine False _ _ _ = return False
    combine True i j e
      | i >= j = return True
      | otherwise = return $ maybe False (equal e) (csrIndex csr j i)

csrIsSymmetric :: (Storable a, Prim a, Eq a) => CSR a -> Bool
csrIsSymmetric = csrIsSymmetricBy (==)
{-# INLINE csrIsSymmetric #-}
{-# SCC csrIsSymmetric #-}

cooShape :: COO a -> (Int, Int)
cooShape (COO rowIndices columnIndices _) = (dim rowIndices, dim columnIndices)
  where
    dim xs
      | V.length xs == 0 = 0
      | otherwise = fromIntegral $ 1 + V.foldl' max 0 xs
{-# INLINE cooShape #-}

cooDim :: COO a -> Int
cooDim coo
  | n == m = n
  | otherwise = error $ "matrix is not square: " <> show n <> " != " <> show m
  where
    (n, m) = cooShape coo
{-# INLINE cooDim #-}

-- extractDiagonal :: (Num a, Prim a, Storable a) => COO a -> (COO a, a)
-- extractDiagonal (COO rowIndices columnIndices elements) = runST $ do
--   let n = V.length rowIndices
--   rowIndices' <- MV.unsafeNew n
--   columnIndices' <- MV.unsafeNew n
--   elements' <- MV.unsafeNew n
--   let go !acc !offset !i
--         | i < n =
--           if indexVector rowIndices i == indexVector columnIndices i
--             then go (acc + indexVector elements i) offset (i + 1)
--             else do
--               writeVector rowIndices' offset $ indexVector rowIndices i
--               writeVector columnIndices' offset $ indexVector columnIndices i
--               writeVector elements' offset $ indexVector elements i
--               go acc (offset + 1) (i + 1)
--         | otherwise = return (acc, offset)
--   (trace, n') <- go 0 0 0
--   coo' <-
--     COO <$> V.unsafeFreeze (MV.unsafeTake n' rowIndices')
--       <*> V.unsafeFreeze (MV.unsafeTake n' columnIndices')
--       <*> V.unsafeFreeze (MV.unsafeTake n' elements')
--   return (coo', trace)
-- {-# SCC extractDiagonal #-}

fromCOO :: Maybe Int -> COO a -> CSR a
fromCOO dim coo = CSR elements columnIndices rowIndices
  where
    inBounds :: Vector Word32 -> Int -> Bool
    inBounds indices k = V.all (\x -> fromIntegral x < k) indices
    !n = case dim of
      Just d ->
        if inBounds (cooRowIndices coo) d && inBounds (cooColumnIndices coo) d
          then d
          else error $ "rowIndices or columnIndices are out of bounds"
      Nothing -> cooDim coo
    !elements = cooData coo
    !columnIndices = cooColumnIndices coo
    !rowIndices = runST $ do
      rs <- MV.replicate (n + 1) 0
      V.forM_ (cooRowIndices coo) $ \i ->
        modifyVector rs (+ 1) (fromIntegral i + 1)
      loopM_ 0 n $ \i -> do
        r <- readVector rs i
        modifyVector rs (+ r) (i + 1)
      V.unsafeFreeze rs
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
  fields <- replicateM n (uniformRM (-1, 1) gen)
  let coo = zipWith (\(i, j) c -> (fromIntegral i, fromIntegral j, c)) graph couplings
      csr = fromCOO Nothing . fromList $ coo ++ map (\(i, j, c) -> (j, i, c)) coo
  return $ Hamiltonian csr (fromList fields) 0

randomHamiltonian :: RandomGen g => Int -> Double -> g -> (Hamiltonian, g)
randomHamiltonian n p g = runStateGen g (randomHamiltonianM n p)

loadFromCSV :: String -> IO Hamiltonian
loadFromCSV filename = do
  contents <- lines <$> T.readFile filename
  let parse [i, c] = Left (Text.Read.read i, Text.Read.read c)
      parse [i, j, c] = Right (Text.Read.read i, Text.Read.read j, Text.Read.read c)
      parse parts = error $ "Parsing " <> show filename <> " failed: " <> show parts
      numbers = parse . map toString . T.splitOn "," <$> contents
      (coo, diagonal) = extractDiagonal . fromList $ rights numbers
      csr = fromCOO Nothing coo
      fields = runST $ do
        f <- MV.replicate (numberRows csr) 0
        forM_ (lefts numbers) $ \(i, c) -> writeVector f i c
        V.unsafeFreeze f
  return $ Hamiltonian csr fields diagonal
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

mkHamiltonian :: COO Double -> Vector Double -> Hamiltonian
mkHamiltonian matrix field = Hamiltonian (fromCOO (Just numberSpins) matrix') field trace
  where
    numberSpins = V.length field
    (matrix', trace) = extractDiagonal matrix

sa_create_hamiltonian ::
  Word32 ->
  Ptr Word32 ->
  Ptr Word32 ->
  Ptr Double ->
  Word32 ->
  Ptr Double ->
  IO (StablePtr Hamiltonian)
sa_create_hamiltonian numberCouplings rowIndicesPtr columnIndicesPtr dataPtr numberSpins fieldPtr = do
  let fromPtr count p = V.unsafeFromForeignPtr0 <$> newForeignPtr_ p <*> pure count
  fields <- fromPtr (fromIntegral numberSpins) fieldPtr
  matrix <-
    mkCOO <$> fromPtr (fromIntegral numberCouplings) rowIndicesPtr
      <*> fromPtr (fromIntegral numberCouplings) columnIndicesPtr
      <*> fromPtr (fromIntegral numberCouplings) dataPtr
  let hamiltonian = mkHamiltonian matrix fields
  -- (matrix, trace) <-
  --   fmap extractDiagonal $
  --     mkCOO <$> fromPtr (fromIntegral numberCouplings) rowIndicesPtr
  --       <*> fromPtr (fromIntegral numberCouplings) columnIndicesPtr
  --       <*> fromPtr (fromIntegral numberCouplings) dataPtr
  -- let hamiltonian = Hamiltonian (fromCOO (Just (fromIntegral numberSpins)) matrix) fields trace
  unless (csrIsSymmetric (hamiltonianExchange hamiltonian)) $ error "Hamiltonian is not symmetric"
  newStablePtr hamiltonian

foreign export ccall sa_create_hamiltonian :: Word32 -> Ptr Word32 -> Ptr Word32 -> Ptr Double -> Word32 -> Ptr Double -> IO (StablePtr Hamiltonian)

sa_destroy_hamiltonian :: StablePtr Hamiltonian -> IO ()
sa_destroy_hamiltonian = freeStablePtr

foreign export ccall sa_destroy_hamiltonian :: StablePtr Hamiltonian -> IO ()

configurationFromPtr :: Int -> Ptr Word64 -> IO Configuration
configurationFromPtr n p = do
  let blocks = (n + 63) `div` 64
  v <- newAlignedPinnedPrimArray blocks
  copyBytes (mutablePrimArrayContents v) p $ blocks * sizeOf (0 :: Word64)
  unsafeFreeze $ MutableConfiguration v

sa_find_ground_state ::
  -- | Hamiltonian
  StablePtr Hamiltonian ->
  -- | Initial configuration. If @nullPtr@, a random initial configuration will
  -- be chosen
  Ptr Word64 ->
  -- | Seed for the random number generator
  Word32 ->
  -- | Number sweeps
  Word32 ->
  -- | Initial β
  Double ->
  -- | Final β
  Double ->
  -- | Best configuration
  Ptr Word64 ->
  -- | Current energy history
  Ptr Double ->
  -- | Best energy history
  Ptr Double ->
  IO ()
sa_find_ground_state _hamiltonian xPtr₀ seed _sweeps β₀ β₁ xPtr currentEPtr bestEPtr = do
  hamiltonian <- deRefStablePtr _hamiltonian
  let n = dimension hamiltonian
      g₀ = CongruentialState seed
      sweeps = fromIntegral _sweeps
      options = SimulationOptions hamiltonian (linearSchedule β₀ β₁ sweeps) sweeps
  (x₀, g₁) <-
    if xPtr₀ == nullPtr
      then return $ randomConfiguration' n g₀
      else do
        x <- configurationFromPtr n xPtr₀
        return (x, g₀)
  let (_, (Configuration xBest), eCurrent, eBest, _) = runAnnealing options x₀ g₁
  copyPrimArrayToPtr xPtr xBest 0 (sizeofPrimArray xBest)
  let sizeOfDouble = let x = x :: Double in sizeOf x
  when (currentEPtr /= nullPtr) $
    V.unsafeWith eCurrent $ \src ->
      copyBytes currentEPtr src (V.length eCurrent * sizeOfDouble)
  when (bestEPtr /= nullPtr) $
    V.unsafeWith eBest $ \src ->
      copyBytes bestEPtr src (V.length eBest * sizeOfDouble)

foreign export ccall sa_find_ground_state :: StablePtr Hamiltonian -> Ptr Word64 -> Word32 -> Word32 -> Double -> Double -> Ptr Word64 -> Ptr Double -> Ptr Double -> IO ()
