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
-- import Data.Char (intToDigit)
import Data.Foldable (maximum)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Generic.Mutable
import Data.Vector.Unboxed (MVector, Unbox, Vector)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
-- import qualified GHC.Show (show)
-- import Numeric (showIntAtBase)

-- import qualified Language.C.Inline as C
import System.Random.MWC
import System.Random.Stateful
import qualified Text.Read
import Prelude hiding (init, words)

-- | Spin configuration
newtype Configuration = Configuration (Vector Word64)
  deriving stock (Eq, Show)

-- | Mutable spin configuration
newtype MutableConfiguration s = MutableConfiguration (MVector s Word64)

-- | Simulation state
data MutableState s = MutableState
  { currentConfiguration :: {-# UNPACK #-} !(MutableConfiguration s),
    bestConfiguration :: {-# UNPACK #-} !(MutableConfiguration s),
    currentEnergyHistory :: {-# UNPACK #-} !(MVector s Double),
    bestEnergyHistory :: {-# UNPACK #-} !(MVector s Double),
    energyChanges :: {-# UNPACK #-} !(MVector s Double)
  }

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

unsafeFreeze :: PrimMonad m => MutableConfiguration (PrimState m) -> m Configuration
unsafeFreeze (MutableConfiguration v) = Configuration <$> U.unsafeFreeze v
{-# INLINE unsafeFreeze #-}

-- freeze :: PrimMonad m => MutableConfiguration (PrimState m) -> m Configuration
-- freeze (MutableConfiguration v) = Configuration <$> U.freeze v

thaw :: PrimMonad m => Configuration -> m (MutableConfiguration (PrimState m))
thaw (Configuration v) = MutableConfiguration <$> U.thaw v

copy :: PrimMonad m => MutableConfiguration (PrimState m) -> MutableConfiguration (PrimState m) -> m ()
copy (MutableConfiguration target) (MutableConfiguration source) = MU.unsafeCopy target source

-- | Get the value of @i@'th spin (either @-1@ or @+1@).
unsafeIndex :: Configuration -> Int -> Int8
unsafeIndex !(Configuration words) !i =
  assert (block < U.length words) $
    case testBit (U.unsafeIndex words block) rest of
      False -> -1
      True -> 1
  where
    block = i `div` 64
    rest = i `mod` 64
{-# INLINE unsafeIndex #-}

-- | Flip @i@'th spin.
unsafeFlip :: PrimMonad m => MutableConfiguration (PrimState m) -> Int -> m ()
unsafeFlip !(MutableConfiguration words) !i =
  assert (block < MU.length words) $
    MU.unsafeModify words (\x -> complementBit x rest) block
  where
    block = i `div` 64
    rest = i `mod` 64
{-# INLINE unsafeFlip #-}

-- | Return number of rows in the matrix
numberRows :: CSR a -> Int
numberRows csr = U.length (csrRowIndices csr) - 1

csrRow :: Unbox a => CSR a -> Int -> SparseVector a
csrRow !(CSR elements columnIndices rowIndices) !i =
  assert (i < U.length rowIndices - 1) $
    SparseVector
      (U.slice begin (end - begin) columnIndices)
      (U.slice begin (end - begin) elements)
  where
    begin = fromIntegral $ U.unsafeIndex rowIndices i
    end = fromIntegral $ U.unsafeIndex rowIndices (i + 1)
{-# INLINE csrRow #-}

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

elemBy :: Unbox a => (a -> a -> Bool) -> (Int, a) -> SparseVector a -> Bool
elemBy !eq (!i', !c') (SparseVector indices elements) =
  U.any id $ U.zipWith (\i c -> i == fromIntegral i' && eq c c') indices elements

anyElement :: Unbox a => (Int -> Int -> a -> Bool) -> CSR a -> Bool
anyElement predicate (CSR elements columnIndices rowIndices) =
  loopAny 0 (U.length rowIndices - 1) $ \i ->
    let begin = fromIntegral $ U.unsafeIndex rowIndices i
        end = fromIntegral $ U.unsafeIndex rowIndices (i + 1)
     in loopAny begin end $ \k ->
          predicate i (fromIntegral $ U.unsafeIndex columnIndices k) (U.unsafeIndex elements k)

allElements :: Unbox a => (Int -> Int -> a -> Bool) -> CSR a -> Bool
allElements predicate = not . anyElement predicate'
  where
    predicate' !i !j !c = not (predicate i j c)

isSymmetricBy :: Unbox a => (a -> a -> Bool) -> CSR a -> Bool
isSymmetricBy eq matrix = allElements check matrix
  where
    check !i !j !c = elemBy eq (i, c) $ csrRow matrix j

isSymmetric :: (Unbox a, Eq a) => CSR a -> Bool
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
  (Unbox a, Num a) =>
  -- | Matrix M
  CSR a ->
  -- | Vector v
  Configuration ->
  -- | Row index i
  Int ->
  -- | ∑ⱼMᵢⱼvⱼ
  a
matrixVectorProductElement !(CSR elements columnIndices rowIndices) !v !i =
  assert (i < U.length rowIndices - 1) $ go 0 begin end
  where
    begin = fromIntegral $ U.unsafeIndex rowIndices i
    end = fromIntegral $ U.unsafeIndex rowIndices (i + 1)
    go !acc !k !n
      | k < n =
        let !j = fromIntegral $ U.unsafeIndex columnIndices k
            !coupling = U.unsafeIndex elements k
            !σⱼ = fromIntegral $ unsafeIndex v j
         in go (acc + coupling * σⱼ) (k + 1) n
      | otherwise = acc

-- | Compute energy of a classical spin configuration
computeEnergy :: Hamiltonian -> Configuration -> Double
computeEnergy !hamiltonian !configuration =
  hamiltonianOffset hamiltonian + goColumn 0 0 (U.length (csrRowIndices matrix) - 1)
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
  U.generate n (energyChangeUponFlip hamiltonian configuration)
  where
    n = numberRows $ hamiltonianExchange hamiltonian

computeEnergyChangesReference :: Hamiltonian -> Configuration -> Vector Double
computeEnergyChangesReference hamiltonian configuration =
  U.generate n (energyChangeUponFlipReference hamiltonian configuration)
  where
    n = numberRows $ hamiltonianExchange hamiltonian

createMutableState :: PrimMonad m => Int -> Hamiltonian -> Configuration -> m (MutableState (PrimState m))
createMutableState !numberSweeps !hamiltonian !x = do
  _energyChanges <- U.thaw $ computeEnergyChanges hamiltonian x
  _currentConfiguration <- thaw x
  _bestConfiguration <- thaw x
  _currentEnergyHistory <- MU.new (numberSweeps + 1)
  _bestEnergyHistory <- MU.new (numberSweeps + 1)
  let e = computeEnergy hamiltonian x
  MU.write _currentEnergyHistory 0 e
  MU.write _bestEnergyHistory 0 e
  return $
    MutableState
      _currentConfiguration
      _bestConfiguration
      _currentEnergyHistory
      _bestEnergyHistory
      _energyChanges

shuffleVector ::
  (PrimMonad m, Data.Vector.Generic.Mutable.MVector v a, StatefulGen g m) => v (PrimState m) a -> g -> m ()
shuffleVector vector gen = go vector (Data.Vector.Generic.Mutable.length vector - 1)
  where
    go !v !j
      | j > 0 = do
        k <- uniformRM (0, j) gen
        Data.Vector.Generic.Mutable.unsafeSwap v k j
        go v (j - 1)
      | otherwise = return ()

randomPermutation :: (HasCallStack, PrimMonad m, StatefulGen g m) => Int -> g -> m (U.Vector Int)
randomPermutation size gen
  | size >= 0 = do
    vector' <- U.unsafeThaw $ U.fromList [0 .. (size - 1)]
    shuffleVector vector' gen
    U.unsafeFreeze vector'
  | otherwise = error $ "invalid size: " <> show size

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

recomputeEnergyChanges ::
  PrimMonad m =>
  Hamiltonian ->
  MVector (PrimState m) Double ->
  Int ->
  Configuration ->
  m ()
recomputeEnergyChanges hamiltonian deltaEnergies i configuration =
  assert (i < dimension hamiltonian) $ do
    let !σᵢ = fromIntegral $ unsafeIndex configuration i
        !(pre :: Double) = -8 * σᵢ
        !(SparseVector indices elements) = csrRow (hamiltonianExchange hamiltonian) i
        update !j !coupling =
          let !j' = fromIntegral j
              !σⱼ = fromIntegral $ unsafeIndex configuration j'
           in MU.unsafeModify deltaEnergies (\δE -> δE + pre * coupling * σⱼ) j'
    MU.unsafeModify deltaEnergies negate i
    U.zipWithM_ update indices elements
{-# SPECIALIZE recomputeEnergyChanges :: Hamiltonian -> MVector s Double -> Int -> Configuration -> ST s () #-}
{-# SPECIALIZE recomputeEnergyChanges :: Hamiltonian -> MVector RealWorld Double -> Int -> Configuration -> IO () #-}

-- {-# SCC recomputeEnergyChanges #-}

-- U.unsafeFreeze deltaEnergies >>= \frozenDeltaEnergies ->
--   assert (allClose frozenDeltaEnergies (computeEnergyChanges hamiltonian configuration)) $
--     return ()

runStep ::
  (StatefulGen g m, PrimMonad m) =>
  Hamiltonian ->
  Double ->
  Int ->
  Int ->
  g ->
  MutableState (PrimState m) ->
  m ()
runStep !hamiltonian !β !sweep !i !gen !s = do
  δEᵢ <- MU.unsafeRead (energyChanges s) i
  accept <-
    if δEᵢ < 0
      then return True
      else do
        u <- uniformRM (0, 2) gen
        return $ exp (- β * δEᵢ) > u
  when accept $ do
    energy <- (+ δEᵢ) <$> MU.unsafeRead (currentEnergyHistory s) sweep
    bestEnergy <- MU.unsafeRead (bestEnergyHistory s) sweep
    let x = currentConfiguration s
    unsafeFlip x i
    x' <- unsafeFreeze x
    recomputeEnergyChanges hamiltonian (energyChanges s) i x'
    -- let e' = computeEnergy hamiltonian x'
    -- trace (show energy <> ", " <> show e') $
    --   assert (isCloseDouble energy e') $
    --     return ()
    MU.unsafeWrite (currentEnergyHistory s) sweep energy
    when (energy < bestEnergy) $ do
      copy (bestConfiguration s) x
      MU.unsafeWrite (bestEnergyHistory s) sweep energy
{-# SPECIALIZE runStep :: Hamiltonian -> Double -> Int -> Int -> Gen s -> MutableState s -> ST s () #-}
{-# SPECIALIZE runStep :: Hamiltonian -> Double -> Int -> Int -> Gen RealWorld -> MutableState RealWorld -> IO () #-}

runSweep ::
  (StatefulGen g m, PrimMonad m) =>
  MVector (PrimState m) Int ->
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
  U.unsafeFreeze order >>= \order' -> U.forM_ order' $ \i ->
    runStep hamiltonian β sweep i gen s

anneal' ::
  (StatefulGen g m, PrimMonad m) =>
  SimulationOptions ->
  Configuration ->
  g ->
  m (Configuration, Configuration, Vector Double, Vector Double)
anneal' options init gen = do
  s <- createMutableState (optionsNumberSweeps options) (optionsHamiltonian options) init
  order <- U.unsafeThaw $ U.generate (dimension (optionsHamiltonian options)) id
  loopM_ 0 (optionsNumberSweeps options) $ \i -> do
    runSweep order (optionsHamiltonian options) (optionsSchedule options i) i gen s
    MU.unsafeWrite (currentEnergyHistory s) (i + 1) =<< MU.unsafeRead (currentEnergyHistory s) i
    MU.unsafeWrite (bestEnergyHistory s) (i + 1) =<< MU.unsafeRead (bestEnergyHistory s) i
  xCurrent <- unsafeFreeze $ currentConfiguration s
  xBest <- unsafeFreeze $ bestConfiguration s
  eCurrent <- U.unsafeFreeze $ currentEnergyHistory s
  eBest <- U.unsafeFreeze $ bestEnergyHistory s
  return (xCurrent, xBest, eCurrent, eBest)

randomConfiguration :: StatefulGen g m => Int -> g -> m Configuration
randomConfiguration n gen = do
  let blocks = n `div` 64
      rest = n `mod` 64
  completeWords <- replicateM blocks (uniformM gen)
  if rest > 0
    then do
      lastWord <- uniformRM (0, 2 ^ rest - 1) gen
      return . Configuration . U.fromList $ completeWords ++ [lastWord]
    else return . Configuration . U.fromList $ completeWords

anneal ::
  (StatefulGen g m, PrimMonad m) =>
  SimulationOptions ->
  g ->
  m (Configuration, Configuration, Vector Double, Vector Double)
anneal options gen
  | numberSpins == 0 = error $ "invalid number of spins: " <> show numberSpins
  | otherwise = do
    init <- randomConfiguration numberSpins gen
    anneal' options init gen
  where
    numberSpins = dimension (optionsHamiltonian options)

bruteForceSolve :: Hamiltonian -> (Double, Configuration)
bruteForceSolve hamiltonian
  | n == 0 = (0, Configuration $ U.singleton 0)
  | n < 64 =
    let x = Configuration (U.singleton 0)
        e = computeEnergy hamiltonian x
     in go 1 x e
  | otherwise = error "it is unfeasible to iterate over more than 2⁶³ spin configurations"
  where
    n = dimension hamiltonian
    end = 2 ^ n
    go !x !xBest !eBest
      | x < end =
        let !configuration = Configuration (U.singleton x)
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

csr2coo :: Unbox a => CSR a -> [(Int, Int, a)]
csr2coo (CSR elements columnIndices rowIndices) =
  zip3 rows (fromIntegral <$> U.toList columnIndices) (U.toList elements)
  where
    ns = U.toList $ U.zipWith (-) (U.tail rowIndices) (U.init rowIndices)
    rows = do
      (n, i) <- zip ns [0 ..]
      replicate (fromIntegral n) i

coo2csr :: Unbox a => [(Int, Int, a)] -> CSR a
coo2csr coo = CSR elements columnIndices rowIndices
  where
    noValue f (a₁, a₂, _) (b₁, b₂, _) = f (a₁, a₂) (b₁, b₂)
    !coo' = nubBySorted (noValue (==)) $ sortBy (noValue compare) coo
    !elements = U.fromList $ (\(_, _, x) -> x) <$> coo'
    !columnIndices = U.fromList $ (\(_, j, _) -> fromIntegral j) <$> coo'
    !n
      | null coo' = 0
      | otherwise = (+ 1) . maximum $ (\(i, j, _) -> max i j) <$> coo'
    !rowIndices = runST $ do
      rs <- MU.replicate (n + 1) 0
      forM_ coo' $ \(i, _, _) ->
        MU.modify rs (+ 1) (i + 1)
      forM_ [0 .. (n - 1)] $ \i -> do
        r <- MU.read rs i
        MU.modify rs (+ r) (i + 1)
      U.unsafeFreeze rs

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
