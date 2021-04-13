module Main (main) where

import Control.Monad.ST
import qualified Data.Vector.Unboxed as U
import Physics.Ising
import System.Random.MWC
import Test.Hspec

mkMatrix :: [Double] -> [Int] -> [Int] -> CSR Double
mkMatrix csrData csrColumnIndices csrRowIndices =
  CSR (U.fromList csrData) (U.fromList $ fromIntegral <$> csrColumnIndices) (U.fromList $ fromIntegral <$> csrRowIndices)

roundTo :: RealFrac a => a -> Int -> a
roundTo x n = (fromInteger . round $ x * (10 ^ n)) / (10.0 ^^ n)

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

      -- let options = SimulationOptions h (linearSchedule 0.1 0.5 10) 10
      -- print $ anneal options (c 3)

      bruteForceSolve h `shouldBe` (-12, c 2)
  describe "nubBySorted" $ do
    it "removed duplicates" $ do
      let eq :: Int -> Int -> Bool
          eq = (==)
      nubBySorted eq [] `shouldBe` []
      nubBySorted eq [1] `shouldBe` [1]
      nubBySorted eq [1, 2] `shouldBe` [1, 2]
      nubBySorted eq [1, 1] `shouldBe` [1]
      nubBySorted eq [1, 2, 2, 0, 5, 0] `shouldBe` [1, 2, 0, 5, 0]
  describe "Erdos-Renyi graphs" $ do
    it "constructs random graphs" $ do
      let graph₁ = runST $ initialize (U.singleton 1) >>= graphErdosRenyi 3 0.0
          graph₂ = runST $ initialize (U.singleton 2) >>= graphErdosRenyi 3 1.0
      graph₁ `shouldBe` []
      graph₂ `shouldBe` [(0, 1), (0, 2), (1, 2)]
  describe "anneal" $ do
    it "solves systems of 0 spins" $ do
      let (eComputed, eExpected) = runST $ do
            gen <- initialize (U.singleton 46)
            h <- randomHamiltonian 16 0.8 gen
            let sweeps = 1000
                options = SimulationOptions h (linearSchedule 0.1 3.0 sweeps) sweeps
                (e', _) = bruteForceSolve h
            (e, _) <- anneal options gen
            return (e, e')
      print (eComputed, eExpected)
      roundTo eComputed 7 `shouldBe` roundTo eExpected 7
