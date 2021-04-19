module Main (main) where

import Control.Monad.ST
import Data.Primitive.PrimArray
import qualified Data.Text.IO as T
import qualified Data.Vector.Storable as V
import Physics.Ising
import System.Mem
import System.Random.MWC

main :: IO ()
main = do
  !h <- loadFromCSV "kagome_16.csv"
  -- performGC
  let (_, _, current, best) = runST $ do
        !gen <- createCongruential 46 -- initialize (V.singleton 46)
        -- h <- randomHamiltonian 5 0.8 gen
        let sweeps = 2000
            options = SimulationOptions h (exponentialSchedule 0.1 10000.0 sweeps) sweeps
        anneal options gen
  -- withFile "output_16.dat" WriteMode $ \handle ->
  --   V.forM_ (V.zip current best) $ \(e₁, e₂) ->
  --     T.hPutStrLn handle $ show e₁ <> "," <> show e₂
  print $ indexPrimArray best 2000
