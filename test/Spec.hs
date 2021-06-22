module Main (main) where

import Control.Monad.ST
import qualified Data.Vector.Storable as V
import Physics.Ising
import System.Random.MWC
import Test.Hspec

mkMatrix :: [Double] -> [Int] -> [Int] -> CSR Double
mkMatrix elements columnIndices rowIndices =
  CSR (fromList elements) (fromList $ fromIntegral <$> columnIndices) (fromList $ fromIntegral <$> rowIndices)

roundTo :: RealFrac a => a -> Int -> a
roundTo x n = (fromInteger . round $ x * (10 ^ n)) / (10.0 ^^ n)

main :: IO ()
main = hspec $ do
  describe "nubBySorted" $ do
    it "removed duplicates" $ do
      let eq :: Int -> Int -> Bool
          eq = (==)
      nubBySorted eq [] `shouldBe` []
      nubBySorted eq [1] `shouldBe` [1]
      nubBySorted eq [1, 2] `shouldBe` [1, 2]
      nubBySorted eq [1, 1] `shouldBe` [1]
      nubBySorted eq [1, 2, 2, 0, 5, 0] `shouldBe` [1, 2, 0, 5, 0]
  describe "CSR" $ do
    it "computes expectation values" $ do
      let cooMatrix =
            fromList $
              [ (0, 1, 5),
                (0, 2, 1),
                (1, 0, 5),
                (1, 2, 2),
                (2, 0, 1),
                (2, 1, 2)
              ]
          matrix =
            mkMatrix
              [5, 1, 5, 2, 1, 2]
              [1, 2, 0, 2, 0, 1]
              [0, 2, 4, 6]
          h = Hamiltonian matrix (V.replicate 3 0) 0
          c x = Configuration $ fromList [x]
      fromCOO Nothing cooMatrix `shouldBe` matrix
      csrIsSymmetric matrix `shouldBe` True
      computeEnergy h (c 0) `shouldBe` 16
      computeEnergy h (c 1) `shouldBe` (-8)
      computeEnergy h (c 2) `shouldBe` (-12)
      computeEnergy h (c 3) `shouldBe` 4
      computeEnergy h (c 4) `shouldBe` 4
      computeEnergy h (c 5) `shouldBe` (-12)
      computeEnergy h (c 6) `shouldBe` (-8)
      computeEnergy h (c 7) `shouldBe` 16
      computeEnergyChanges h (c 0) `shouldBe` (V.fromList [-24, -28, -12])
      computeEnergyChanges h (c 1) `shouldBe` (V.fromList [24, 12, -4])
      bruteForceSolve h `shouldBe` (c 2, -12)
  describe "Erdos-Renyi graphs" $ do
    it "constructs random graphs" $ do
      let graph₁ = runST $ initialize (V.singleton 1) >>= graphErdosRenyi 3 0.0
          graph₂ = runST $ initialize (V.singleton 2) >>= graphErdosRenyi 3 1.0
      graph₁ `shouldBe` []
      graph₂ `shouldBe` [(0, 1), (0, 2), (1, 2)]
  describe "anneal" $ do
    it "solves systems of 5 spins" $ do
      forM_ [46, 47, 48, 49] $ \seed -> do
        gen <- initialize (V.singleton seed)
        h <- randomHamiltonianM 5 0.8 gen
        let sweeps = 1000
            options = SimulationOptions h (linearSchedule 0.1 6.0 sweeps) sweeps
            (_, eExpected) = bruteForceSolve h
            (x, eComputed) = simpleGroundState options seed
        roundTo (computeEnergy h x) 10 `shouldBe` roundTo eComputed 10
        roundTo eComputed 7 `shouldBe` roundTo eExpected 7
    it "solves systems of 10 spins" $ do
      forM_ [51, 52, 53] $ \seed -> do
        gen <- initialize (V.singleton seed)
        h <- randomHamiltonianM 10 0.8 gen
        let sweeps = 1000
            options = SimulationOptions h (linearSchedule 0.1 6.0 sweeps) sweeps
            (_, eExpected) = bruteForceSolve h
            (x, eComputed) = simpleGroundState options seed
        roundTo (computeEnergy h x) 10 `shouldBe` roundTo eComputed 10
        roundTo eComputed 7 `shouldBe` roundTo eExpected 7

{-
-- describe "computeEnergyChanges" $ do
--   it "works" $ do
--     h <- loadFromCSV "kagome_12.csv"
--     isSymmetric (hamiltonianExchange h) `shouldBe` True
--     forM_ [5] $ \x -> do
--       let c = Configuration (U.singleton x)
--       computeEnergyChanges h c `shouldBe` computeEnergyChangesReference h c

  it "solves Kagome 12" $ do
    h <- loadFromCSV "kagome_12.csv"
    let e = runST $ do
          gen <- initialize (U.singleton 46)
          let sweeps = 1000
              options = SimulationOptions h (exponentialSchedule 0.1 100.0 sweeps) sweeps
          (_, _, _, history) <- anneal options gen
          return (U.last history)
    -- roundTo (fst (bruteForceSolve h)) 7 `shouldBe` (-8)
    roundTo e 7 `shouldBe` (-8)
-}
