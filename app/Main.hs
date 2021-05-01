module Main (main) where

import Physics.Ising

main :: IO ()
main = do
  !h <- loadFromCSV "kagome_16.csv"
  print $ csrIsSymmetric (hamiltonianExchange h)
  let sweeps = 2000
      options = SimulationOptions h (exponentialSchedule 0.1 20000.0 sweeps) sweeps
      (_, !energy) = simpleGroundState options 46
  print $ energy
