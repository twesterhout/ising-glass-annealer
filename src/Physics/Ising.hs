module Physics.Ising
  ( Configuration (..),
    Hamiltonian (..),
    CSR (..),
    SimulationOptions (..),
    computeEnergy,
    computeEnergyChanges,
    anneal,
    linearSchedule,
  )
where

import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.State.Strict
import Data.Bits
import Data.Char (intToDigit)
import qualified Data.Vector.Generic.Mutable
import Data.Vector.Unboxed (MVector, Unbox, Vector)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
-- import Debug.Trace
import qualified GHC.Show (show)
import Numeric (showIntAtBase)
import System.Random.MWC
import System.Random.Stateful

newtype Configuration = Configuration {configurationWords :: Vector Word64}
  deriving stock (Eq, Show)

newtype MutableConfiguration s = MutableConfiguration {mutableConfigurationWords :: MVector s Word64}

unsafeFreeze :: PrimMonad m => MutableConfiguration (PrimState m) -> m Configuration
unsafeFreeze (MutableConfiguration v) = Configuration <$> U.unsafeFreeze v

freeze :: PrimMonad m => MutableConfiguration (PrimState m) -> m Configuration
freeze (MutableConfiguration v) = Configuration <$> U.freeze v

thaw :: PrimMonad m => Configuration -> m (MutableConfiguration (PrimState m))
thaw (Configuration v) = MutableConfiguration <$> U.thaw v

unsafeIndex :: HasCallStack => Configuration -> Int -> Int8
unsafeIndex (Configuration words) i =
  case testBit (words U.! (i `div` 64)) (i `mod` 64) of
    False -> -1
    True -> 1

unsafeFlip :: HasCallStack => PrimMonad m => MutableConfiguration (PrimState m) -> Int -> m ()
unsafeFlip (MutableConfiguration words) i =
  MU.modify words (\x -> complementBit x (i `mod` 64)) (i `div` 64)

data CSR a = CSR
  { csrData :: Vector a,
    csrColumnIndices :: Vector Word32,
    csrRowIndices :: Vector Word32
  }

numberRows :: CSR a -> Int
numberRows csr = U.length (csrRowIndices csr) - 1

linearSchedule :: Double -> Double -> Int -> (Int -> Double)
linearSchedule β₀ β₁ numberSweeps
  | numberSweeps == 0 = \_ -> 0
  | otherwise = \i -> β₀ + c * fromIntegral i
  where
    c = (β₁ - β₀) / fromIntegral (numberSweeps - 1)

data SparseMatrix

data SimulationState = SimulationState
  { ssMatrix :: SparseMatrix
  }

data GeneratorState s

chooseSpin :: GeneratorState s -> ST s Int
chooseSpin = undefined

uniform :: GeneratorState s -> ST s Double
uniform = undefined

data MutableState s = MutableState
  { currentConfiguration :: MutableConfiguration s,
    currentEnergy :: Double,
    bestConfiguration :: Configuration,
    bestEnergy :: Double,
    energyChanges :: MVector s Double
  }

foldlCSR' :: (b -> Int -> Int -> a -> b) -> b -> CSR a -> b
foldlCSR' = undefined

rowBounds :: CSR a -> Int -> (Int, Int)
rowBounds matrix i = (fromIntegral $ indptr U.! i, fromIntegral $ indptr U.! (i + 1))
  where
    indptr = csrRowIndices matrix

matrixVectorProductElement :: (HasCallStack, Unbox a, Num a) => CSR a -> Configuration -> Int -> a
matrixVectorProductElement (CSR !elements !columnIndices !rowIndices) !v !i = go 0 begin end
  where
    !begin = fromIntegral $ rowIndices U.! i
    !end = fromIntegral $ rowIndices U.! (i + 1)
    go !acc !k !n
      | k < n =
        let !j = fromIntegral $ columnIndices U.! k
            !coupling = elements U.! k
            !σⱼ = fromIntegral $ unsafeIndex v j
         in go (acc + coupling * σⱼ) (k + 1) n
      | otherwise = acc

computeEnergy :: HasCallStack => Hamiltonian -> Configuration -> Double
computeEnergy !hamiltonian !configuration = goColumn 0 0 (U.length (csrRowIndices matrix) - 1)
  where
    !matrix = hamiltonianExchange hamiltonian
    goColumn !acc !i !n
      | i < n =
        let !σᵢ = fromIntegral $ unsafeIndex configuration i
            !acc' = acc + σᵢ * matrixVectorProductElement matrix configuration i
         in goColumn acc' (i + 1) n
      | otherwise = acc

energyChangeUponFlip :: HasCallStack => Hamiltonian -> Configuration -> Int -> Double
energyChangeUponFlip hamiltonian configuration i =
  -4 * σᵢ * matrixVectorProductElement (hamiltonianExchange hamiltonian) configuration i
  where
    !σᵢ = fromIntegral $ unsafeIndex configuration i

computeEnergyChanges :: HasCallStack => Hamiltonian -> Configuration -> Vector Double
computeEnergyChanges hamiltonian configuration =
  trace ("n = " <> show n) $
    U.generate n (energyChangeUponFlip hamiltonian configuration)
  where
    n = numberRows $ hamiltonianExchange hamiltonian

createMutableState :: PrimMonad m => Hamiltonian -> Configuration -> m (MutableState (PrimState m))
createMutableState hamiltonian x = do
  x' <- thaw x
  deltaEnergies <- U.unsafeThaw $ computeEnergyChanges hamiltonian x
  let energy = computeEnergy hamiltonian x
  return $ MutableState x' energy x energy deltaEnergies

data SimulationOptions = SimulationOptions
  { optionsHamiltonian :: Hamiltonian,
    optionsSchedule :: Int -> Double,
    optionsNumberSweeps :: Int
  }

data Hamiltonian = Hamiltonian {hamiltonianExchange :: CSR Double}

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

forSpinsM_ :: (PrimMonad m, StatefulGen g m) => Int -> g -> (Int -> m ()) -> m ()
forSpinsM_ numberSpins gen action = do
  vector <- randomPermutation numberSpins gen
  U.forM_ vector action

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

recomputeEnergyChanges :: PrimMonad m => Hamiltonian -> MVector (PrimState m) Double -> Int -> Configuration -> m ()
recomputeEnergyChanges hamiltonian deltaEnergies i configuration = do
  MU.modify deltaEnergies negate i
  forRowM_ (hamiltonianExchange hamiltonian) i $ \j coupling ->
    let !σⱼ = fromIntegral $ unsafeIndex configuration j
     in MU.modify deltaEnergies (\(!δE) -> δE + pre * coupling * σⱼ) j
  where
    !σᵢ = fromIntegral $ unsafeIndex configuration i
    !(pre :: Double) = (-4) * σᵢ

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
      trace ("Accepted: E = " <> show energy) $ return ()
      lift $ unsafeFlip (currentConfiguration state) i
      lift $ recomputeEnergyChanges hamiltonian (energyChanges state) i =<< unsafeFreeze (currentConfiguration state)
      case energy < bestEnergy state of
        True -> do
          bestConfiguration' <- freeze (currentConfiguration state)
          put $
            state
              { currentEnergy = energy,
                bestConfiguration = bestConfiguration',
                bestEnergy = energy
              }
        False -> put $ state {currentEnergy = energy}
    else do
      trace ("Rejected: E = " <> show (currentEnergy state)) $ return ()

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

anneal :: SimulationOptions -> Configuration -> (Double, Configuration)
anneal options init = runST $ do
  gen <- initialize (U.singleton 42)
  state <- createMutableState (optionsHamiltonian options) init
  trace ("Initially: E = " <> show (currentEnergy state)) $ return ()
  let simulation = loopM_ 0 (optionsNumberSweeps options) $ \i ->
        runSweep (optionsHamiltonian options) (optionsSchedule options i) gen
  state' <- execStateT simulation state
  return (bestEnergy state', bestConfiguration state')

graphErdosRenyi :: StatefulGen g m => Int -> Double -> g -> m [(Int, Int)]
graphErdosRenyi n p
  | n > 0 && 0 <= p && p <= 1 = undefined
  | n <= 0 = error $ "invalid n: " <> show n

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
