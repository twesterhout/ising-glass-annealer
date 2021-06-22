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
    return $ mkHamiltonian matrix field
  let sweeps = 10000
      (β₀, β₁) = estimateBetas hamiltonian
      options = SimulationOptions hamiltonian (exponentialSchedule β₀ β₁ sweeps) sweeps
  print (β₀, β₁)
  let (_, !energy) = simpleGroundState options 48
  print energy
