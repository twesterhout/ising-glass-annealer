{-# LANGUAGE ScopedTypeVariables #-}

module Physics.Ising
  ( -- * Annealing
    Configuration (..),
    SimulationOptions (..),
    anneal,
    anneal',
    groundState,
    bruteForceSolve,

    -- * Matrices
    COO (..),
    CSR (..),
    extractDiagonal,
    fromCOO,
    csrIsSymmetric,
    csrIndex,
    nubBySorted,

    -- * Hamiltonian
    Hamiltonian (..),
    loadFromCSV,
    graphErdosRenyi,
    randomHamiltonian,
    computeEnergy,
    computeEnergyChanges,

    -- * Annealing schedule
    linearSchedule,
    exponentialSchedule,

    -- * Random number generation
    CongruentialState (..),
    createCongruential,

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
import Data.Foldable (maximum)
import Data.Primitive.ByteArray (MutableByteArray (..), mutableByteArrayContents)
import Data.Primitive.PrimArray
import qualified Data.Primitive.Ptr as P
import Data.Primitive.Types (Prim)
import qualified Data.Text as T
import qualified Data.Text.IO as T
-- import qualified Data.Vector.Generic.Mutable
import Data.Vector.Storable (Storable, Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.C.Types
import Foreign.Ptr
import Foreign.StablePtr
import GHC.Exts
import GHC.Float
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

-- | Spin configuration.
--
-- Every spin is represented by a single bit: @bit=0@ means @spin=-1@ and
-- @bit=1@ means @spin=1@.
newtype Configuration = Configuration (PrimArray Word64)
  deriving newtype (Eq, Show)

-- | Mutable spin configuration.
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

newtype CongruentialState s = CongruentialState (MutablePrimArray s Word32)

createCongruential :: PrimMonad m => Word32 -> m (CongruentialState (PrimState m))
createCongruential seed = do
  v <- newPrimArray 1
  writePrimArray v 0 seed
  return (CongruentialState v)

nextWord :: PrimMonad m => CongruentialState (PrimState m) -> m Word32
nextWord (CongruentialState s) = do
  writePrimArray s 0 =<< nextState <$> readPrimArray s 0
  readPrimArray s 0
  where
    nextState :: Word32 -> Word32
    nextState !x = 69069 * x + 1234567
{-# INLINE nextWord #-}
{-# SCC nextWord #-}

wordsTo64Bit :: Word32 -> Word32 -> Word64
wordsTo64Bit x y = (fromIntegral x `shiftL` 32) .|. fromIntegral y
{-# INLINE wordsTo64Bit #-}

instance (s ~ PrimState m, PrimMonad m) => StatefulGen (CongruentialState s) m where
  uniformWord8 s = fromIntegral <$> nextWord s
  uniformWord16 s = fromIntegral <$> nextWord s
  uniformWord32 s = nextWord s
  {-# INLINE uniformWord32 #-}
  uniformWord64 s = wordsTo64Bit <$> nextWord s <*> nextWord s
  {-# INLINE uniformWord64 #-}
  uniformShortByteString = error "uniformShortByteString not implemented for CongruentialState"

-- | Simulation state
data MutableState s = MutableState
  { currentConfiguration :: {-# UNPACK #-}!(MutableConfiguration s),
    bestConfiguration :: {-# UNPACK #-}!(MutableConfiguration s),
    currentEnergyHistory :: !(MutablePrimArray s Double),
    bestEnergyHistory :: !(MutablePrimArray s Double),
    energyChanges :: !(MutablePrimArray s Double)
  }

-- |
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

data Hamiltonian = Hamiltonian
  { hamiltonianExchange :: {-# UNPACK #-} !(CSR Double),
    hamiltonianOffset :: {-# UNPACK #-} !Double
  }

binarySearch :: Ord a => (Int -> a) -> Int -> Int -> a -> Maybe Int
binarySearch atIndex = go
  where
    go lower upper z
      | lower >= upper = Nothing
      | lower < upper =
        let !i = lower + (upper - lower) `div` 2
            !x = atIndex i
         in case compare x z of
              LT -> go i upper z
              GT -> go lower i z
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

-- | Computes one element of a matrix-vector product ∑ⱼMᵢⱼvⱼ
matrixVectorProductElement ::
  (Storable a, Num a) =>
  -- | Matrix M
  CSR a ->
  -- | Vector v
  Configuration ->
  -- | Row index i
  Int ->
  -- | ∑ⱼMᵢⱼvⱼ
  a
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

-- | Compute how much the energy would change if the @i@'th spin was flipped.
energyChangeUponFlip :: Hamiltonian -> Configuration -> Int -> Double
energyChangeUponFlip hamiltonian configuration i =
  -4 * σᵢ * matrixVectorProductElement (hamiltonianExchange hamiltonian) configuration i
  where
    !σᵢ = fromIntegral $ unsafeIndex configuration i

-- | Compute 'energyChangeUponFlip' for every spin.
computeEnergyChanges :: Hamiltonian -> Configuration -> Vector Double
computeEnergyChanges hamiltonian configuration =
  V.generate n (energyChangeUponFlip hamiltonian configuration)
  where
    n = numberRows $ hamiltonianExchange hamiltonian

vectorToPrimArray :: (Prim a, Storable a) => Vector a -> PrimArray a
vectorToPrimArray = primArrayFromList . V.toList

createMutableState :: PrimMonad m => Int -> Hamiltonian -> Configuration -> m (MutableState (PrimState m))
createMutableState !numberSweeps !hamiltonian !x = do
  _energyChanges <- unsafeThawPrimArray . vectorToPrimArray $ computeEnergyChanges hamiltonian x
  _currentConfiguration <- thaw x
  _bestConfiguration <- thaw x
  _currentEnergyHistory <- newAlignedPinnedPrimArray (numberSweeps + 1)
  _bestEnergyHistory <- newAlignedPinnedPrimArray (numberSweeps + 1)
  let e = computeEnergy hamiltonian x
  writePrimArray _currentEnergyHistory 0 e
  writePrimArray _bestEnergyHistory 0 e
  return $
    MutableState
      _currentConfiguration
      _bestConfiguration
      _currentEnergyHistory
      _bestEnergyHistory
      _energyChanges

unsafeMutablePrimArrayPtr :: forall a s. MutablePrimArray s a -> Ptr a
unsafeMutablePrimArrayPtr (MutablePrimArray v) =
  (castPtr :: Ptr Word8 -> Ptr a) $ mutableByteArrayContents (MutableByteArray v)
{-# INLINE unsafeMutablePrimArrayPtr #-}

unsafeSwap :: PrimMonad m => MutablePrimArray (PrimState m) Int -> Int -> Int -> m ()
unsafeSwap v !i !j = do
  let !p = unsafeMutablePrimArrayPtr v
  a <- P.readOffPtr p i
  b <- P.readOffPtr p j
  P.writeOffPtr p i b
  P.writeOffPtr p j a
  return ()
{-# INLINE unsafeSwap #-}

unbiasedWordMult32Exclusive :: forall g m. StatefulGen g m => Word32 -> g -> m Word32
unbiasedWordMult32Exclusive !r !g = go
  where
    t :: Word32
    t = (- r) `mod` r -- Calculates 2^32 `mod` r!!!
    go = do
      (m :: Word64) <- (fromIntegral r *) <$> fromIntegral <$> uniformWord32 g
      if fromIntegral m >= t then return (fromIntegral $! m `shiftR` 32) else go
{-# INLINE unbiasedWordMult32Exclusive #-}
{-# SCC unbiasedWordMult32Exclusive #-}

shuffleVector :: (StatefulGen g m, PrimMonad m) => MutablePrimArray (PrimState m) Int -> g -> m ()
shuffleVector !v !gen = go n
  where
    !n = sizeofMutablePrimArray v - 1
    go !j
      | j > 0 = do
        unsafeSwap v j =<< fromIntegral <$> {-# SCC "uniform" #-} unbiasedWordMult32Exclusive (fromIntegral j) gen
        go (j - 1)
      | otherwise = return ()
{-# INLINEABLE shuffleVector #-}
{-# SCC shuffleVector #-}

-- randomPermutation :: (HasCallStack, PrimMonad m, StatefulGen g m) => Int -> g -> m (Vector Int)
-- randomPermutation size gen
--   | size >= 0 = do
--     vector' <- V.unsafeThaw $ V.fromList [0 .. (size - 1)]
--     shuffleVector vector' gen
--     V.unsafeFreeze vector'
--   | otherwise = error $ "invalid size: " <> show size

loopM_ :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
loopM_ !begin !end f = go begin
  where
    go !i
      | i < end = f i >> go (i + 1)
      | otherwise = return ()

-- isCloseDouble :: Double -> Double -> Bool
-- isCloseDouble a b = abs (a - b) < 1.0e-5
--
-- allClose :: Vector Double -> Vector Double -> Bool
-- allClose a b = U.all id $ U.zipWith isCloseDouble a b

{-
zipWithM'_ ::
  PrimMonad m =>
  (Int -> Double -> m ()) ->
  Vector Word32 ->
  Vector Double ->
  m ()
zipWithM'_ !f !xs !ys = go 0
  where
    !n = V.length xs
    go !k
      | k < n = do
        let !xₖ = fromIntegral $ V.unsafeIndex xs k
            !yₖ = V.unsafeIndex ys k
            !k' = k + 1
        !_ <- {-# SCC "r" #-} f xₖ yₖ
        go k'
      | otherwise = return ()
    {-# INLINE go #-}
{-# INLINE zipWithM'_ #-}
{-# SPECIALIZE zipWithM'_ :: (Int -> Double -> IO ()) -> Vector Word32 -> Vector Double -> IO () #-}
{-# SPECIALIZE zipWithM'_ :: (Int -> Double -> ST s ()) -> Vector Word32 -> Vector Double -> ST s () #-}
{-# SCC zipWithM'_ #-}
-}

foreign import ccall unsafe "unsafe_get_spin"
  unsafe_get_spin :: Ptr Word64 -> CUInt -> CInt

foreign import ccall unsafe "recompute_energy_changes"
  recompute_energy_changes :: Ptr Double -> Ptr Word32 -> Ptr Word32 -> CUInt -> Ptr Word64 -> Ptr Double -> IO ()

recompute_energy_changes' ::
  PrimMonad m =>
  Ptr Double ->
  Ptr Word32 ->
  Ptr Word32 ->
  Int ->
  Configuration ->
  MutablePrimArray (PrimState m) Double ->
  m ()
recompute_energy_changes' csr_data csr_column_indices csr_row_indices i bits energy_changes = do
  writePrimArray energy_changes i =<< (negate <$> readPrimArray energy_changes i)
  go begin
  where
    begin :: Int
    begin = fromIntegral $ P.indexOffPtr csr_row_indices i
    end :: Int
    end = fromIntegral $ P.indexOffPtr csr_row_indices (i + 1)
    pre :: Double
    pre = fromIntegral $ (-8) * unsafeIndex bits i
    go !k
      | k < end = do
        let !j = fromIntegral $ P.indexOffPtr csr_column_indices k
            !coupling = P.indexOffPtr csr_data k
            !σⱼ = fromIntegral $ unsafeIndex bits j
        !e <- readPrimArray energy_changes j
        writePrimArray energy_changes j (e + pre * coupling * σⱼ)
        go (k + 1)
      | otherwise = return ()
{-# SCC recompute_energy_changes' #-}
{-# INLINEABLE recompute_energy_changes' #-}

recomputeEnergyChanges ::
  forall m.
  PrimBase m =>
  Hamiltonian ->
  MutablePrimArray (PrimState m) Double ->
  Int ->
  Configuration ->
  m ()
recomputeEnergyChanges hamiltonian deltaEnergies i bits = do
  let (CSR elements columnIndices rowIndices) = hamiltonianExchange hamiltonian
  unsafeIOToPrim $
    V.unsafeWith elements $ \csr_data ->
      V.unsafeWith columnIndices $ \csr_column_indices ->
        V.unsafeWith rowIndices $ \csr_row_indices -> do
          (unsafePrimToIO :: m () -> IO ()) $
            -- V.unsafeWith words $ \bits ->
            -- MV.unsafeWith (coerce deltaEnergies) $ \delta_energies ->
            -- {-# SCC "recompute_energy_changes" #-}
            -- trace "Calling recompute_energy_changes..." $
            recompute_energy_changes'
              csr_data
              csr_column_indices
              csr_row_indices
              i
              bits
              deltaEnergies
{-# SCC recomputeEnergyChanges #-}
{-# INLINEABLE recomputeEnergyChanges #-}

-- recomputeEnergyChanges ::
--   PrimMonad m =>
--   Hamiltonian ->
--   MVector (PrimState m) Double ->
--   Int ->
--   Configuration ->
--   m ()
-- recomputeEnergyChanges hamiltonian deltaEnergies i configuration =
--   assert (i < dimension hamiltonian) $ do
--     let !σᵢ = fromIntegral $ unsafeIndex configuration i
--         !(pre :: Double) = -8 * σᵢ
--         !(SparseVector !indices !elements) = csrRow (hamiltonianExchange hamiltonian) i
--         update !j !coupling =
--           {-# SCC "update" #-}
--           let !σⱼ = fromIntegral $ unsafeIndex configuration j
--            in MV.unsafeModify deltaEnergies (\δE -> δE + pre * coupling * σⱼ) j
--     MV.unsafeModify deltaEnergies negate i
--     zipWithM'_ update indices elements
-- {-# SPECIALIZE recomputeEnergyChanges :: Hamiltonian -> MVector s Double -> Int -> Configuration -> ST s () #-}
-- {-# SPECIALIZE recomputeEnergyChanges :: Hamiltonian -> MVector RealWorld Double -> Int -> Configuration -> IO () #-}
-- {-# SCC recomputeEnergyChanges #-}

-- U.unsafeFreeze deltaEnergies >>= \frozenDeltaEnergies ->
--   assert (allClose frozenDeltaEnergies (computeEnergyChanges hamiltonian configuration)) $
--     return ()

generateAcceptanceProbabilities ::
  (StatefulGen g m, PrimMonad m) =>
  Double ->
  MutablePrimArray (PrimState m) Double ->
  g ->
  m ()
generateAcceptanceProbabilities β probabilities gen = action 0
  where
    !n = sizeofMutablePrimArray probabilities
    action !i
      | i < n = do
        u <- float2Double <$> uniformFloat01M gen
        writePrimArray probabilities i $ (1 / β) * log (2 * u)
        action (i + 1)
      | otherwise = return ()

-- undefined

shouldAccept :: StatefulGen g m => g -> Double -> Double -> m Bool
shouldAccept gen β !δEᵢ
  | δEᵢ >= 0 =
    let !p = 0.5 * exp (- β * δEᵢ)
     in (p >) . float2Double <$> uniformFloat01M gen
  | otherwise = return True
{-# INLINE shouldAccept #-}
{-# SCC shouldAccept #-}

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

anneal' ::
  (StatefulGen g m, PrimBase m) =>
  SimulationOptions ->
  Configuration ->
  g ->
  m (Configuration, Configuration, PrimArray Double, PrimArray Double)
anneal' options init gen = do
  s <- createMutableState (optionsNumberSweeps options) (optionsHamiltonian options) init
  order <- unsafeThawPrimArray $ initialOrder (dimension (optionsHamiltonian options))
  loopM_ 0 (optionsNumberSweeps options) $ \i -> do
    runSweep order (optionsHamiltonian options) (optionsSchedule options i) i gen s
    writePrimArray (currentEnergyHistory s) (i + 1) =<< readPrimArray (currentEnergyHistory s) i
    writePrimArray (bestEnergyHistory s) (i + 1) =<< readPrimArray (bestEnergyHistory s) i
  xCurrent <- unsafeFreeze $ currentConfiguration s
  xBest <- unsafeFreeze $ bestConfiguration s
  eCurrent <- unsafeFreezePrimArray $ currentEnergyHistory s
  eBest <- unsafeFreezePrimArray $ bestEnergyHistory s
  return (xCurrent, xBest, eCurrent, eBest)

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

groundState ::
  (StatefulGen g m, PrimBase m) =>
  SimulationOptions ->
  g ->
  m (Configuration, Double)
groundState options gen = do
  (_, x, _, es) <- anneal options gen
  return (x, indexPrimArray es (optionsNumberSweeps options - 1))

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

skipUniformly :: (HasCallStack, StatefulGen g m) => Double -> g -> [a] -> m [a]
skipUniformly p gen list
  | p < 0 || p > 1 = error $ "invalid probability p: " <> show p
  -- Special case p = 1 because uniformRM returns numbers in [0, 1] rather than [0, 1)
  | p == 1 = return list
  | otherwise = do
    us <- replicateM (length list) $ uniformRM (0, 1) gen
    return . map fst . filter ((< p) . snd) . zip list $ us

graphErdosRenyi :: (HasCallStack, StatefulGen g m) => Int -> Double -> g -> m [(Int, Int)]
graphErdosRenyi n p gen
  | n < 0 = error $ "invalid number of nodes n: " <> show n
  | n == 0 = return []
  | otherwise = do
    let edges = [(i, j) | i <- [0 .. (n - 2)], j <- [(i + 1) .. (n - 1)]]
    skipUniformly p gen edges

randomHamiltonian :: StatefulGen g m => Int -> Double -> g -> m Hamiltonian
randomHamiltonian n p gen = do
  graph <- graphErdosRenyi n p gen
  couplings <- replicateM (length graph) (uniformRM (-1, 1) gen)
  let coo = zipWith (\(i, j) c -> (fromIntegral i, fromIntegral j, c)) graph couplings
      csr = fromCOO . fromList $ coo ++ map (\(i, j, c) -> (j, i, c)) coo
  return $ Hamiltonian csr 0

loadFromCSV :: String -> IO Hamiltonian
loadFromCSV filename = do
  contents <- lines <$> T.readFile filename
  let parse [i, j, c] = (Text.Read.read i, Text.Read.read j, Text.Read.read c)
      parse parts = error $ "Parsing " <> show filename <> " failed: " <> show parts
      (coo, diagonal) = extractDiagonal . fromList $ parse . map toString . T.splitOn "," <$> contents
  return $ Hamiltonian (fromCOO coo) diagonal
{-# SCC loadFromCSV #-}

create_hamiltonian ::
  Word32 ->
  Ptr Double ->
  Ptr Word32 ->
  Ptr Word32 ->
  Ptr Double ->
  IO (StablePtr Hamiltonian)
create_hamiltonian n dataPtr columnIndicesPtr rowIndicesPtr _ = do
  undefined

foreign export ccall create_hamiltonian :: Word32 -> Ptr Double -> Ptr Word32 -> Ptr Word32 -> Ptr Double -> IO (StablePtr Hamiltonian)
