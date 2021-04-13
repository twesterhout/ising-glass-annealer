module Physics.Ising
  ( Configuration (..),
    Hamiltonian (..),
    CSR (..),
    SimulationOptions (..),
    computeEnergy,
    computeEnergyChanges,
    anneal,
    bruteForceSolve,
    linearSchedule,
    nubBySorted,
    graphErdosRenyi,
    randomHamiltonian,
  )
where

import Control.Exception (assert)
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.State.Strict
import Data.Bits
import Data.Char (intToDigit)
import Data.Foldable (maximum)
import qualified Data.Vector.Generic.Mutable
import Data.Vector.Unboxed (MVector, Unbox, Vector)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import qualified GHC.Show (show)
import Numeric (showIntAtBase)
import System.Random.MWC
import System.Random.Stateful

newtype Configuration = Configuration {configurationWords :: Vector Word64}
  deriving stock (Eq, Show)

newtype MutableConfiguration s = MutableConfiguration {mutableConfigurationWords :: MVector s Word64}

data MutableState s = MutableState
  { currentConfiguration :: MutableConfiguration s,
    currentEnergy :: Double,
    bestConfiguration :: MutableConfiguration s,
    bestEnergy :: Double,
    energyChanges :: MVector s Double
  }

data SimulationOptions = SimulationOptions
  { optionsHamiltonian :: !Hamiltonian,
    optionsSchedule :: !(Int -> Double),
    optionsNumberSweeps :: !Int
  }

-- | Sparse matrix in Compressed Sparse Row (CSR) format.
data CSR a = CSR
  { csrData {-# UNBOX #-} :: !(Vector a),
    csrColumnIndices {-# UNBOX #-} :: !(Vector Word32),
    csrRowIndices {-# UNBOX #-} :: !(Vector Word32)
  }
  deriving stock (Eq, Show)

newtype Hamiltonian = Hamiltonian {hamiltonianExchange :: CSR Double}

unsafeFreeze :: PrimMonad m => MutableConfiguration (PrimState m) -> m Configuration
unsafeFreeze (MutableConfiguration v) = Configuration <$> U.unsafeFreeze v
{-# INLINE unsafeFreeze #-}

freeze :: PrimMonad m => MutableConfiguration (PrimState m) -> m Configuration
freeze (MutableConfiguration v) = Configuration <$> U.freeze v

thaw :: PrimMonad m => Configuration -> m (MutableConfiguration (PrimState m))
thaw (Configuration v) = MutableConfiguration <$> U.thaw v

copy :: PrimMonad m => MutableConfiguration (PrimState m) -> Configuration -> m ()
copy (MutableConfiguration target) (Configuration source) = MU.copy target =<< U.unsafeThaw source

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
computeEnergy !hamiltonian !configuration = goColumn 0 0 (U.length (csrRowIndices matrix) - 1)
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
  U.generate n (energyChangeUponFlip hamiltonian configuration)
  where
    n = numberRows $ hamiltonianExchange hamiltonian

createMutableState :: PrimMonad m => Hamiltonian -> Configuration -> m (MutableState (PrimState m))
createMutableState hamiltonian !x = do
  x' <- thaw x
  x'' <- thaw x
  deltaEnergies <- U.unsafeThaw $ computeEnergyChanges hamiltonian x
  let energy = computeEnergy hamiltonian x
  return $ MutableState x' energy x'' energy deltaEnergies

shuffleVector ::
  (PrimMonad m, Data.Vector.Generic.Mutable.MVector v a, StatefulGen g m) => v (PrimState m) a -> g -> m ()
shuffleVector vector gen = go vector (Data.Vector.Generic.Mutable.length vector - 1)
  where
    go !v !j
      | j > 0 = do
        k <- uniformRM (0, j) gen
        Data.Vector.Generic.Mutable.swap v k j
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

forRowM_ :: (Monad m, Unbox a) => CSR a -> Int -> (Int -> a -> m ()) -> m ()
forRowM_ matrix i f =
  loopM_ (fromIntegral $ (csrRowIndices matrix) U.! i) (fromIntegral $ (csrRowIndices matrix) U.! (i + 1)) $ \k ->
    let !j = fromIntegral $ (csrColumnIndices matrix) U.! k
        !c = (csrData matrix) U.! k
     in f j c

recomputeEnergyChangesReference :: Hamiltonian -> Int -> Configuration -> Vector Double
recomputeEnergyChangesReference hamiltonian i configuration =
  let configuration' = runST $ do
        mutableConfiguration <- thaw configuration
        unsafeFlip mutableConfiguration i
        unsafeFreeze mutableConfiguration
   in computeEnergyChanges hamiltonian configuration'

recomputeEnergyChanges :: (HasCallStack, PrimMonad m) => Hamiltonian -> MVector (PrimState m) Double -> Int -> Configuration -> m ()
recomputeEnergyChanges hamiltonian deltaEnergies i configuration
  | i >= dimension hamiltonian = error $ "index out of bounds: " <> show i
  | otherwise = do
    -- MU.copy deltaEnergies =<< U.thaw (computeEnergyChanges hamiltonian configuration)
    let !σᵢ = fromIntegral $ unsafeIndex configuration i
        !(pre :: Double) = (-8) * σᵢ
    MU.unsafeModify deltaEnergies negate i
    forRowM_ (hamiltonianExchange hamiltonian) i $ \j coupling ->
      let !σⱼ = fromIntegral $ unsafeIndex configuration j
       in MU.modify deltaEnergies (\(!δE) -> δE + pre * coupling * σⱼ) j

-- U.unsafeFreeze deltaEnergies >>= \frozenDeltaEnergies ->
--   let ref = computeEnergyChanges hamiltonian configuration
--    in trace (show frozenDeltaEnergies <> ", " <> show ref) $
--         assert (frozenDeltaEnergies == ref) $
--           return ()

runStep ::
  (StatefulGen g m, PrimMonad m) =>
  Hamiltonian ->
  Double ->
  Int ->
  g ->
  StateT (MutableState (PrimState m)) m ()
runStep hamiltonian β i gen = do
  state <- get
  (u :: Double) <- lift $ uniformRM (0, 2) gen
  δEᵢ <- lift $ MU.read (energyChanges state) i
  if (δEᵢ < 0 || exp (- β * δEᵢ) > u)
    then do
      let energy = currentEnergy state + δEᵢ
          x = currentConfiguration state
      lift $ unsafeFlip x i
      lift $ recomputeEnergyChanges hamiltonian (energyChanges state) i =<< unsafeFreeze x
      case energy < bestEnergy state of
        True -> do
          copy (bestConfiguration state) =<< unsafeFreeze x
          put $ state {currentEnergy = energy, bestEnergy = energy}
        False -> put $ state {currentEnergy = energy}
    else do
      return ()

runSweep ::
  (StatefulGen g m, PrimMonad m) =>
  Hamiltonian ->
  Double ->
  g ->
  StateT (MutableState (PrimState m)) m ()
runSweep hamiltonian β gen = do
  order <- lift $ randomPermutation n gen
  U.forM_ order $ \i ->
    runStep hamiltonian β i gen
  where
    n = numberRows (hamiltonianExchange hamiltonian)

anneal' :: (StatefulGen g m, PrimMonad m) => SimulationOptions -> Configuration -> g -> m (Double, Configuration)
anneal' options init gen = do
  state <- createMutableState (optionsHamiltonian options) init
  let simulation = loopM_ 0 (optionsNumberSweeps options) $ \i ->
        runSweep (optionsHamiltonian options) (optionsSchedule options i) gen
  state' <- execStateT simulation state
  let energy = bestEnergy state'
  configuration <- freeze $ bestConfiguration state'
  return (energy, configuration)

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

anneal :: (StatefulGen g m, PrimMonad m) => SimulationOptions -> g -> m (Double, Configuration)
anneal options gen
  | numberSpins == 0 = return (0, Configuration (U.singleton 0))
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

coo2csr :: (HasCallStack, Unbox a) => [(Int, Int, a)] -> CSR a
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
  return $ Hamiltonian . coo2csr $ coo ++ map (\(i, j, c) -> (j, i, c)) coo

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
