{-# LANGUAGE RankNTypes #-}

module ForeignLibrary () where

import Control.Concurrent (runInUnboundThread)
import Control.DeepSeq
import Control.Monad (forM_)
import Data.Int
import qualified Data.Primitive.Ptr as P
import Data.Word
import Foreign.Ptr
import Physics.Ising

sa_anneal_f64 ::
  -- Hamiltonian
  Int32 ->
  Ptr Double ->
  Ptr Int32 ->
  Ptr Int32 ->
  Ptr Double ->
  -- | Seed for the random number generator
  Word32 ->
  -- | Number repetitions
  Int32 ->
  -- | Number sweeps
  Int32 ->
  -- | Initial β
  Float ->
  -- | Final β
  Float ->
  -- | Array of best configurations (i.e. a 2D array of Word64)
  Ptr Word64 ->
  -- | Array of best energies
  Ptr Double ->
  IO ()
sa_anneal_f64 c_n elts colIdxs rowIdxs field seed c_repetitions c_sweeps β0 β1 xArr eArr =
  runInUnboundThread $ do
    -- setNumCapabilities =<< getNumProcessors
    -- putStrLn "Running sa_anneal ..."
    -- print =<< getNumCapabilities
    -- unless (repetitions >= 1) $
    --   error $
    --     "invalid number of repetitions: " <> show repetitions
    -- hamiltonian <- deRefStablePtr hamiltonianPtr
    let n = fromIntegral c_n
        sweeps = fromIntegral c_sweeps
        repetitions = fromIntegral c_repetitions
        matrix = CSR' n elts colIdxs rowIdxs
        hamiltonian = Hamiltonian' matrix field
        schedule = exponentialSchedule β0 β1 sweeps
        inputs =
          flip fmap [0 .. repetitions - 1] $ \i ->
            let g = CongruentialState (seed + fromIntegral i)
                stride = (n + 63) `div` 64 -- stride in Word64
                p = xArr `P.advancePtr` (i * stride)
             in (Bits' p, g)
    -- (βEstimated₀, βEstimated₁) = estimateBetas hamiltonian
    -- β₀ <- if βPtr₀ == nullPtr then return βEstimated₀ else peek βPtr₀
    -- β₁ <- if βPtr₁ == nullPtr then return βEstimated₁ else peek βPtr₁
    -- x₀ <-
    --   if xPtr₀ == nullPtr
    --     then pure Nothing
    --     else Just <$> configurationFromPtr n xPtr₀
    -- let options = SimulationOptions hamiltonian (exponentialSchedule β₀ β₁ sweeps') sweeps'
    results <- annealParallel' schedule sweeps hamiltonian inputs
    forM_ (zip [0 ..] results) $ \(i, (e, _)) -> do
      P.writeOffPtr eArr i e

-- writeManyConfigurations xPtr (fmap fst results)
-- pokeArray ePtr (fmap snd results)

foreign export ccall "sa_anneal_f64"
  sa_anneal_f64 ::
    Int32 ->
    Ptr Double ->
    Ptr Int32 ->
    Ptr Int32 ->
    Ptr Double ->
    Word32 ->
    Int32 ->
    Int32 ->
    Float ->
    Float ->
    Ptr Word64 ->
    Ptr Double ->
    IO ()

sa_compute_energy_f64 ::
  Int32 ->
  Ptr Double ->
  Ptr Int32 ->
  Ptr Int32 ->
  Ptr Double ->
  Ptr Word64 ->
  IO Double
sa_compute_energy_f64 c_n elts colIdxs rowIdxs field x = do
  let n = fromIntegral c_n
      matrix = CSR' n elts colIdxs rowIdxs
      hamiltonian = Hamiltonian' matrix field
  e <- computeEnergyM hamiltonian (Bits' x)
  e `deepseq` pure e

foreign export ccall "sa_compute_energy_f64"
  sa_compute_energy_f64 :: Int32 -> Ptr Double -> Ptr Int32 -> Ptr Int32 -> Ptr Double -> Ptr Word64 -> IO Double

sa_estimate_betas_f64 ::
  Int32 ->
  Ptr Double ->
  Ptr Int32 ->
  Ptr Int32 ->
  Ptr Double ->
  Ptr Double ->
  Ptr Double ->
  IO ()
sa_estimate_betas_f64 c_n elts colIdxs rowIdxs field minBeta maxBeta = do
  let n = fromIntegral c_n
      hamiltonian = Hamiltonian' (CSR' n elts colIdxs rowIdxs) field
      (beta0, beta1) = estimateBetas hamiltonian
  P.writeOffPtr minBeta 0 beta0
  P.writeOffPtr maxBeta 0 beta1

foreign export ccall "sa_estimate_betas_f64"
  sa_estimate_betas_f64 :: Int32 -> Ptr Double -> Ptr Int32 -> Ptr Int32 -> Ptr Double -> Ptr Double -> Ptr Double -> IO ()

sa_greedy_solve_f64 ::
  -- Hamiltonian
  Int32 ->
  Ptr Double ->
  Ptr Int32 ->
  Ptr Int32 ->
  Ptr Double ->
  -- Configuration
  Ptr Word64 ->
  IO Double
sa_greedy_solve_f64 c_n elts colIdxs rowIdxs field bitsPtr = do
  let n = fromIntegral c_n
      matrix = CSR' n elts colIdxs rowIdxs
      hamiltonian = Hamiltonian' matrix field
  greedySolve hamiltonian (Bits' bitsPtr)

foreign export ccall "sa_greedy_solve_f64"
  sa_greedy_solve_f64 :: Int32 -> Ptr Double -> Ptr Int32 -> Ptr Int32 -> Ptr Double -> Ptr Word64 -> IO Double