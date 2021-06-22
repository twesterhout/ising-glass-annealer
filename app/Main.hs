module Main (main) where

import qualified Data.HDF5 as H5
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Physics.Ising

main :: IO ()
main = do
  hamiltonian <- H5.withFile' "app/problem_kagome_12.h5" H5.ReadOnly $ \file -> do
    (rowIndices :: Vector Int32) <- H5.readDataset =<< H5.openDataset @Text file "row_indices"
    (columnIndices :: Vector Int32) <- H5.readDataset =<< H5.openDataset @Text file "col_indices"
    (elements :: Vector Double) <- H5.readDataset =<< H5.openDataset @Text file "elements"
    let !matrix = mkCOO (V.map fromIntegral rowIndices) (V.map fromIntegral columnIndices) elements
    (field :: Vector Double) <- H5.readDataset =<< H5.openDataset @Text file "field"
    -- (energy :: Vector Double) <- H5.readDataset =<< H5.openDataset @Text file "energy"
    -- print energy
    return $ mkHamiltonian matrix field
  let sweeps = 40000
      -- options = SimulationOptions hamiltonian (linearSchedule 100.0 10000000.0 sweeps) sweeps
      (β₀, β₁) = estimateBetas hamiltonian
      options2 = SimulationOptions hamiltonian (linearSchedule β₀ 400000 sweeps) sweeps
  print (β₀, β₁)
  -- let (_, !energy) = simpleGroundState options 46
  let (_, !energy2) = simpleGroundState options2 49
  -- print energy
  print energy2

-- !h <- loadFromCSV "kagome_16.csv"
-- print $ csrIsSymmetric (hamiltonianExchange h)
-- let sweeps = 2000
--     options = SimulationOptions h (exponentialSchedule 0.1 20000.0 sweeps) sweeps
--     (_, !energy) = simpleGroundState options 46
-- print $ energy
