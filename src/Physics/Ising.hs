{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Physics.Ising
  ( Configuration (..),
    Hamiltonian (..),
    CSR (..),
    csr2coo,
    SimulationOptions (..),
    computeEnergy,
    computeEnergyChanges,
    computeEnergyChangesReference,
    isSymmetric,
    anneal,
    bruteForceSolve,
    linearSchedule,
    exponentialSchedule,
    nubBySorted,
    graphErdosRenyi,
    randomHamiltonian,
    loadFromCSV,
  )
where

import Control.Exception (assert)
import Control.Monad.Identity
import Control.Monad.Primitive
import Control.Monad.ST
-- import Control.Monad.State.Strict
import Data.Bits
import Data.Coerce
-- import Data.Char (intToDigit)
import Data.Foldable (maximum)
import Data.Primitive.ByteArray (MutableByteArray (..), mutableByteArrayContents)
import Data.Primitive.PrimArray
import qualified Data.Primitive.Ptr as P
import Data.Primitive.Types (Prim)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Generic.Mutable
import Data.Vector.Storable (MVector, Storable, Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.C.Types
import Foreign.Ptr
-- import qualified GHC.Show (show)
-- import Numeric (showIntAtBase)

-- import qualified Language.C.Inline as C
import System.Random.MWC
import System.Random.Stateful
import qualified Text.Read
import Prelude hiding (init, words)

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
{-# SCC unsafeIndex #-}

-- | Flip @i@'th spin.
unsafeFlip :: PrimMonad m => MutableConfiguration (PrimState m) -> Int -> m ()
unsafeFlip !(MutableConfiguration v) !i = do
  x <- readPrimArray v block
  writePrimArray v block (complementBit x rest)
  where
    !block = i `div` 64
    !rest = i `mod` 64
{-# INLINE unsafeFlip #-}

-- | Simulation state
data MutableState s = MutableState
  { currentConfiguration :: {-# UNPACK #-} !(MutableConfiguration s),
    bestConfiguration :: {-# UNPACK #-} !(MutableConfiguration s),
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

data SparseVector a = SparseVector {-# UNPACK #-} !(Vector Word32) !(Vector a)

data Hamiltonian = Hamiltonian
  { hamiltonianExchange :: {-# UNPACK #-} !(CSR Double),
    hamiltonianOffset :: {-# UNPACK #-} !Double
  }

-- | Return number of rows in the matrix
numberRows :: CSR a -> Int
numberRows csr = V.length (csrRowIndices csr) - 1

csrRow :: Storable a => CSR a -> Int -> SparseVector a
csrRow !(CSR elements columnIndices rowIndices) !i =
  assert (i < V.length rowIndices - 1) $
    SparseVector
      (V.slice begin (end - begin) columnIndices)
      (V.slice begin (end - begin) elements)
  where
    begin = fromIntegral $ V.unsafeIndex rowIndices i
    end = fromIntegral $ V.unsafeIndex rowIndices (i + 1)
{-# INLINE csrRow #-}
{-# SCC csrRow #-}

loopAnyM :: Monad m => Int -> Int -> (Int -> m Bool) -> m Bool
loopAnyM !begin !end !predicate = go begin
  where
    go !i
      | i < end = do
        flag <- predicate i
        if flag then return True else go (i + 1)
      | otherwise = return False

loopAny :: Int -> Int -> (Int -> Bool) -> Bool
loopAny begin end p = runIdentity $ loopAnyM begin end (return . p)

elemBy :: Storable a => (a -> a -> Bool) -> (Int, a) -> SparseVector a -> Bool
elemBy !eq (!i', !c') (SparseVector indices elements) =
  V.any id $ V.zipWith (\i c -> i == fromIntegral i' && eq c c') indices elements

anyElement :: Storable a => (Int -> Int -> a -> Bool) -> CSR a -> Bool
anyElement predicate (CSR elements columnIndices rowIndices) =
  loopAny 0 (V.length rowIndices - 1) $ \i ->
    let begin = fromIntegral $ V.unsafeIndex rowIndices i
        end = fromIntegral $ V.unsafeIndex rowIndices (i + 1)
     in loopAny begin end $ \k ->
          predicate i (fromIntegral $ V.unsafeIndex columnIndices k) (V.unsafeIndex elements k)

allElements :: Storable a => (Int -> Int -> a -> Bool) -> CSR a -> Bool
allElements predicate = not . anyElement predicate'
  where
    predicate' !i !j !c = not (predicate i j c)

isSymmetricBy :: Storable a => (a -> a -> Bool) -> CSR a -> Bool
isSymmetricBy eq matrix = allElements check matrix
  where
    check !i !j !c = elemBy eq (i, c) $ csrRow matrix j

isSymmetric :: (Storable a, Eq a) => CSR a -> Bool
isSymmetric = isSymmetricBy (==)

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
  hamiltonianOffset hamiltonian + goColumn 0 0 (V.length (csrRowIndices matrix) - 1)
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

energyChangeUponFlipReference :: Hamiltonian -> Configuration -> Int -> Double
energyChangeUponFlipReference hamiltonian configuration i =
  computeEnergy hamiltonian configuration' - computeEnergy hamiltonian configuration
  where
    configuration' = runST $ do
      x <- thaw configuration
      unsafeFlip x i
      unsafeFreeze x

-- | Compute 'energyChangeUponFlip' for every spin.
computeEnergyChanges :: Hamiltonian -> Configuration -> Vector Double
computeEnergyChanges hamiltonian configuration =
  V.generate n (energyChangeUponFlip hamiltonian configuration)
  where
    n = numberRows $ hamiltonianExchange hamiltonian

computeEnergyChangesReference :: Hamiltonian -> Configuration -> Vector Double
computeEnergyChangesReference hamiltonian configuration =
  V.generate n (energyChangeUponFlipReference hamiltonian configuration)
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

unsafeMutablePrimArrayPtr :: forall a s. Prim a => MutablePrimArray s a -> Ptr a
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

shuffleVector :: (StatefulGen g m, PrimMonad m) => MutablePrimArray (PrimState m) Int -> g -> m ()
shuffleVector !v !gen = go (sizeofMutablePrimArray v - 1)
  where
    go !j
      | j > 0 = do
        !k <- {-# SCC "uniform" #-} uniformRM (0, j) gen
        unsafeSwap v j k
        go (j - 1)
      | otherwise = return ()
    {-# INLINE go #-}
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
  {-# SCC "flipI" #-} writePrimArray energy_changes i =<< (negate <$> readPrimArray energy_changes i)
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

shouldAccept :: StatefulGen g m => Double -> Double -> g -> m Bool
shouldAccept !β !δEᵢ !gen
  | δEᵢ >= 0 =
    let !p = {-# SCC "exp" #-} exp (- β * δEᵢ)
     in (p >) <$> force <$> uniformRM (0, 2) gen
  | otherwise = return True
-- return $ p > u
{-# INLINE shouldAccept #-}

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
  let !deltaEnergies = force $ energyChanges s
  !accept <- do
    δEᵢ <- {-# SCC "readDelta" #-} readPrimArray deltaEnergies i
    {-# SCC "shouldAccept" #-} shouldAccept β δEᵢ gen
  when accept $ do
    let x = currentConfiguration s
    unsafeFlip x i
    {-# SCC "_recomputeEnergyChanges" #-} recomputeEnergyChanges hamiltonian (energyChanges s) i =<< unsafeFreeze x
    !oldEnergy <- {-# SCC "readCurrentEnergy" #-} readPrimArray (currentEnergyHistory s) sweep
    !δEᵢ' <- {-# SCC "readDelta'" #-} readPrimArray deltaEnergies i
    let !energy = oldEnergy - δEᵢ'
    {-# SCC "writeCurrentEnergy" #-} writePrimArray (currentEnergyHistory s) sweep energy
    !bestEnergy <- {-# SCC "readBestEnergy" #-} readPrimArray (bestEnergyHistory s) sweep
    when (energy < bestEnergy) $ do
      copy (bestConfiguration s) x
      writePrimArray (bestEnergyHistory s) sweep energy
{-# INLINE runStep #-}
{-# SPECIALIZE runStep :: Hamiltonian -> Double -> Int -> Int -> Gen s -> MutableState s -> ST s () #-}
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

bruteForceSolve :: Hamiltonian -> (Double, Configuration)
bruteForceSolve hamiltonian
  | n == 0 = (0, Configuration $ fromList [0])
  | n < 64 =
    let x = Configuration $ fromList [0]
        e = computeEnergy hamiltonian x
     in go 1 x e
  | otherwise = error "it is unfeasible to iterate over more than 2⁶³ spin configurations"
  where
    n = dimension hamiltonian
    end = 2 ^ n
    go !x !xBest !eBest
      | x < end =
        let !configuration = Configuration (fromList [x])
            !e = computeEnergy hamiltonian configuration
         in if e < eBest
              then go (x + 1) configuration e
              else go (x + 1) xBest eBest
      | otherwise = (eBest, xBest)

nubBySorted :: (a -> a -> Bool) -> [a] -> [a]
nubBySorted eq list = foldr acc [] list
  where
    acc x [] = [x]
    acc x ys@(y : _) = if eq x y then ys else x : ys

removeDiagonal :: Num a => [(Int, Int, a)] -> ([(Int, Int, a)], a)
removeDiagonal coo = (coo', diagonal)
  where
    coo' = filter (\(i, j, _) -> i /= j) coo
    diagonal = sum $ map (\(_, _, c) -> c) $ filter (\(i, j, _) -> i == j) coo

csr2coo :: Storable a => CSR a -> [(Int, Int, a)]
csr2coo (CSR elements columnIndices rowIndices) =
  zip3 rows (fromIntegral <$> V.toList columnIndices) (V.toList elements)
  where
    ns = V.toList $ V.zipWith (-) (V.tail rowIndices) (V.init rowIndices)
    rows = do
      (n, i) <- zip ns [0 ..]
      replicate (fromIntegral n) i

coo2csr :: Storable a => [(Int, Int, a)] -> CSR a
coo2csr coo = CSR elements columnIndices rowIndices
  where
    noValue f (a₁, a₂, _) (b₁, b₂, _) = f (a₁, a₂) (b₁, b₂)
    !coo' = nubBySorted (noValue (==)) $ sortBy (noValue compare) coo
    !elements = V.fromList $ (\(_, _, x) -> x) <$> coo'
    !columnIndices = V.fromList $ (\(_, j, _) -> fromIntegral j) <$> coo'
    !n
      | null coo' = 0
      | otherwise = (+ 1) . maximum $ (\(i, j, _) -> max i j) <$> coo'
    !rowIndices = runST $ do
      rs <- MV.replicate (n + 1) 0
      forM_ coo' $ \(i, _, _) ->
        MV.modify rs (+ 1) (i + 1)
      forM_ [0 .. (n - 1)] $ \i -> do
        r <- MV.read rs i
        MV.modify rs (+ r) (i + 1)
      V.unsafeFreeze rs

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
  let coo = zipWith (\(i, j) c -> (i, j, c)) graph couplings
      csr = coo2csr $ coo ++ map (\(i, j, c) -> (j, i, c)) coo
  return $ Hamiltonian csr 0

loadFromCSV :: String -> IO Hamiltonian
loadFromCSV filename = do
  contents <- lines <$> T.readFile filename
  let parse [i, j, c] = (Text.Read.read i, Text.Read.read j, Text.Read.read c)
      coo = parse . map toString . T.splitOn "," <$> contents
      (coo', diagonal) = removeDiagonal coo
  return $ Hamiltonian (coo2csr coo') diagonal

--
-- i <- chooseSpin
-- u <- uniform
-- let ΔEᵢ = energyChanges !! i
-- if exp(-βΔEᵢ) < u
--  then do
--    currentEnergy <- currentEnergy + ΔEᵢ
--    recomputeEnergyChanges hamiltonian energyChanges currentConfiguration i
--    flipSpin currentConfiguration i
--    if currentEnergy < bestEnergy
--      bestEnergy <- currentEnergy
--      bestConfiguration <- bestConfiguration
--  else
--    return ()
--
