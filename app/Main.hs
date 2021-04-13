module Main (main) where

import qualified Data.Text.IO as T
import qualified Data.Vector.Unboxed as U
import Physics.Ising
import System.Random.MWC

main :: IO ()
main = do
  !h <- loadFromCSV "kagome_12.csv"
  gen <- initialize (U.singleton 46)
  let sweeps = 10000
      options = SimulationOptions h (exponentialSchedule 0.1 5000.0 sweeps) sweeps
  (_, _, current, best) <- anneal options gen
  withFile "output.dat" WriteMode $ \handle ->
    U.forM_ (U.zip current best) $ \(e₁, e₂) ->
      T.hPutStrLn handle $ show e₁ <> "," <> show e₂
  print $ U.last best
