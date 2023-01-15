module Main (main) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.HDF5 as H5
import Data.Int
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Physics.Ising

almostEqual :: Double -> Double -> Bool
almostEqual a b = abs (a - b) < atol + rtol + max (abs a) (abs b)
  where
    !atol = 1.0e-12
    !rtol = 1.0e-9

main :: IO ()
main = do
  let filename = "test/sa_test_kagome_16.h5"
  H5.withFile filename H5.ReadOnly $ \file -> do
    (elts :: Vector Double) <- H5.readDataset =<< H5.open file "elements"
    (colIdxs :: Vector Int32) <- H5.readDataset =<< H5.open file "indices"
    (rowIdxs :: Vector Int32) <- H5.readDataset =<< H5.open file "indptr"
    (field :: Vector Double) <- H5.readDataset =<< H5.open file "field"
    ((H5.Scalar energy) :: H5.Scalar Double) <- H5.readDataset =<< H5.open file "energy"

    let n = V.length rowIdxs - 1
        sweeps = 5196
    liftIO $
      V.unsafeWith elts $ \eltsPtr ->
        V.unsafeWith colIdxs $ \colIdxsPtr ->
          V.unsafeWith rowIdxs $ \rowIdxsPtr ->
            V.unsafeWith field $ \fieldPtr -> do
              x <- MV.new ((n + 63) `div` 64)
              let matrix = CSR' n eltsPtr colIdxsPtr rowIdxsPtr
                  hamiltonian = Hamiltonian' matrix fieldPtr
                  (β0, β1) = estimateBetas hamiltonian
                  schedule = exponentialSchedule (realToFrac β0) (realToFrac β1) sweeps
                  g = CongruentialState 12345
              -- print (β0, β1)
              MV.unsafeWith x $ \xPtr -> do
                (e, _) <- anneal' schedule sweeps hamiltonian (Bits' xPtr) g
                unless (almostEqual e energy) $
                  error $ "Annealing failed to converge: e=" <> show e <> ", e_exact=" <> show energy
  -- (βEstimated₀, βEstimated₁) = estimateBetas hamiltonian
  -- β₀ <- if βPtr₀ == nullPtr then return βEstimated₀ else peek βPtr₀
  -- β₁ <- if βPtr₁ == nullPtr then return βEstimated₁ else peek βPtr₁
  -- x₀ <-
  --   if xPtr₀ == nullPtr
  --     then pure Nothing
  --     else Just <$> configurationFromPtr n xPtr₀
  -- let options = SimulationOptions hamiltonian (exponentialSchedule β₀ β₁ sweeps') sweeps'
  -- results <- annealParallel' schedule sweeps hamiltonian inputs
  -- forM_ (zip [0 ..] results) $ \(i, (e, _)) -> do
  --   P.writeOffPtr eArr i e

  -- liftIO $ print energy
  -- let !matrix = mkCOO (V.map fromIntegral rowIndices) (V.map fromIntegral columnIndices) elements
  -- (trueEnergy :: Vector Double) <- H5.readDataset =<< H5.open file "energy"
  -- print trueEnergy
  -- return $ energy
  -- mkHamiltonian matrix field
  -- print =<< getNumCapabilities

-- let sweeps = 5000
--     (β₀, β₁) = estimateBetas hamiltonian
--     options = SimulationOptions hamiltonian (exponentialSchedule β₀ β₁ sweeps) sweeps
-- print (β₀, β₁)
-- results <- annealParallel options Nothing 4 (CongruentialState 123)
-- print $ fmap snd results
-- let (!x, !energy) = Data.List.minimumBy (comparing snd) results
--     energy' = computeEnergy hamiltonian x
-- -- let (x, !energy) = simpleGroundState options 48
-- --     energy' = computeEnergy hamiltonian x
-- print energy
-- print energy'
-- H5.withFile filename H5.ReadOnly $ \file -> do
--   (x₀ :: Vector Double) <- H5.readDataset =<< H5.open file "ground_state"
--   let overlap = abs $ computeOverlap x₀ x
--       overlap' = abs $ computeOverlap (V.map signum x₀) x
--   print overlap
--   print overlap'
