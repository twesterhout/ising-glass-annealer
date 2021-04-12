module Main (main) where

import qualified Data.Vector.Unboxed as U
import Physics.Ising
import Test.Hspec

mkMatrix :: [Double] -> [Int] -> [Int] -> CSR Double
mkMatrix csrData csrColumnIndices csrRowIndices =
  CSR (U.fromList csrData) (U.fromList $ fromIntegral <$> csrColumnIndices) (U.fromList $ fromIntegral <$> csrRowIndices)

main :: IO ()
main = hspec $ do
  describe "CSR" $ do
    it "computes expectation values" $ do
      let matrix =
            mkMatrix
              [5, 1, 5, 2, 1, 2]
              [1, 2, 0, 2, 0, 1]
              [0, 2, 4, 6]
          h = Hamiltonian matrix
          c x = Configuration $ U.singleton x
      computeEnergy h (c 0) `shouldBe` 16
      computeEnergy h (c 1) `shouldBe` (-8)
      computeEnergy h (c 2) `shouldBe` (-12)
      computeEnergy h (c 3) `shouldBe` 4
      computeEnergy h (c 4) `shouldBe` 4
      computeEnergy h (c 5) `shouldBe` (-12)
      computeEnergy h (c 6) `shouldBe` (-8)
      computeEnergy h (c 7) `shouldBe` 16
      computeEnergyChanges h (c 0) `shouldBe` (U.fromList [-24, -28, -12])
      computeEnergyChanges h (c 1) `shouldBe` (U.fromList [24, 12, -4])

      let options = SimulationOptions h (linearSchedule 0.1 3.0 100) 100
      print $ anneal options (c 3)
