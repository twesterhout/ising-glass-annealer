{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Physics.Ising
  ( -- * Annealing
    Hamiltonian' (..),
    CSR' (..),
    Bits' (..),
    anneal',
    annealParallel',
    computeEnergyM,
    optimizeLocally,
    greedySolve,

    -- * Annealing schedule
    linearSchedule,
    exponentialSchedule,
    estimateBetas,

    -- * Random number generation
    CongruentialState (..),
  )
where

import Control.DeepSeq (NFData, deepseq)
import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.StrictIdentity
import Data.Bits
import Data.Int (Int32)
import Data.Primitive.Ptr (Ptr)
import qualified Data.Primitive.Ptr as P
import Data.Primitive.Types (Prim)
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Generic.Mutable as P
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import Data.Word (Word32, Word64)
import Foreign.Storable (Storable)
import System.Random.Stateful
import UnliftIO.Async (pooledMapConcurrently)
import Prelude hiding (init)

-- | Sparse matrix view in Compressed Sparse Row (CSR) format.
data CSR' i a
  = CSR'
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !(Ptr a)
      {-# UNPACK #-} !(Ptr i)
      {-# UNPACK #-} !(Ptr i)

-- | Sparse matrix view in Coordinate (COO) format.
data COO i a
  = COO
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !(Ptr a)
      {-# UNPACK #-} !(Ptr i)
      {-# UNPACK #-} !(Ptr i)

-- | An array of bits.
newtype Bits' = Bits' (Ptr Word64)

-- | Ising Hamiltonian.
data Hamiltonian' i a
  = Hamiltonian'
      {-# UNPACK #-} !(CSR' i a)
      -- ^ Exchange
      {-# UNPACK #-} !(Ptr a)
      -- ^ External magnetic field

-- | Mutable state for the Simulated Annealing simulation
data MutableState i a
  = MutableState
      {-# UNPACK #-} !(Hamiltonian' i a)
      -- ^ The Hamiltonian
      {-# UNPACK #-} !Bits'
      -- ^ Current spin configuration
      {-# UNPACK #-} !(Ptr a)
      -- ^ Energy changes
      {-# UNPACK #-} !Bits'
      -- ^ Best spin configuration
      {-# UNPACK #-} !(Ptr a)
      -- ^ Energy accumulator

linearSchedule ::
  -- | Initial β
  Double ->
  -- | Final β
  Double ->
  -- | Number of sweeps
  Int ->
  -- | Schedule for linearly decreasing β
  (Int -> Double)
linearSchedule β₀ β₁ numberSweeps
  | numberSweeps < 0 = error $ "invalid number of sweeps: " <> show numberSweeps
  | numberSweeps == 0 = const 0
  | otherwise = \i -> β₀ + c * fromIntegral i
  where
    c = (β₁ - β₀) / fromIntegral (numberSweeps - 1)

exponentialSchedule ::
  RealFloat a =>
  -- | Initial β
  a ->
  -- | Final β
  a ->
  -- | Number of sweeps
  Int ->
  -- | Schedule for linearly decreasing β
  (Int -> a)
exponentialSchedule !β₀ !β₁ !numberSweeps
  | numberSweeps < 0 = error $ "invalid number of sweeps: " <> show numberSweeps
  | numberSweeps == 0 = const 0
  | otherwise = \i -> β₀ * (β₁ / β₀) ** (fromIntegral i / fromIntegral (numberSweeps - 1))

-- | Estimate initial and final β by looking at the Hamiltonian matrix elements.
estimateBetas :: (Prim i, Integral i, Prim a, RealFloat a) => Hamiltonian' i a -> (a, a)
estimateBetas hamiltonian = (minBeta, maxBeta)
  where
    (minDeltaEnergy, maxDeltaEnergy) = energyChangeBounds hamiltonian
    minBeta = log 2 / maxDeltaEnergy
    maxBeta = log 100 / minDeltaEnergy

csrRowFoldM ::
  (Prim i, Integral i, Prim a, Monad m) =>
  CSR' i a ->
  i ->
  (b -> i -> a -> m b) ->
  b ->
  m b
csrRowFoldM (CSR' _ elts colIdxs rowIdxs) i f = fold1 b (< e) (+ 1) combine
  where
    b = P.indexOffPtr rowIdxs (fromIntegral i)
    e = P.indexOffPtr rowIdxs (fromIntegral i + 1)
    combine !acc !k = do !acc' <- f acc j x; pure acc'
      where
        j = P.indexOffPtr colIdxs (fromIntegral k)
        x = P.indexOffPtr elts (fromIntegral k)
{-# INLINE csrRowFoldM #-}

-- csrRowFold ::
--   (Prim i, Integral i, Prim a) => CSR' i a -> i -> (b -> i -> a -> b) -> b -> b
-- csrRowFold matrix i f = runStrictIdentity . csrRowFoldM matrix i f'
--   where
--     f' !z !j !x = let !z' = f z j x in pure z'
-- {-# INLINE csrRowFold #-}

-- csrFold ::
--   (Prim i, Integral i, Prim a, Monad m) =>
--   CSR' i a ->
--   (b -> i -> i -> a -> m b) ->
--   b ->
--   m b
-- csrFold matrix@(CSR' n _ _ _) f = fold1 0 (< fromIntegral n) (+ 1) combine
--   where
--     combine !acc !k = do !acc' <- csrRowFoldM matrix k f acc; pure acc'

energyChangeBounds ::
  forall i a.
  (Prim i, Integral i, Prim a, RealFloat a) =>
  Hamiltonian' i a ->
  (a, a)
energyChangeBounds (Hamiltonian' m@(CSR' n _ _ _) field) = runStrictIdentity $ do
  l <- lower
  u <- upper
  pure (max l (2.220446049250313e-16 * u), u)
  where
    !lower = fold1 0 (< n) (+ 1) combine (1 / 0)
      where
        combine !acc !i = do
          de <- csrRowFoldM m (fromIntegral i) (\ !z !_ x -> pure $ min z (abs x)) (1 / 0)
          pure $ min acc $ 2 * (abs (P.indexOffPtr field i) + de)
    !upper = fold1 0 (< n) (+ 1) combine 0
      where
        combine !acc i = do
          de <- csrRowFoldM m (fromIntegral i) (\ !z !_ x -> pure $ max z (abs x)) 0
          pure $ max acc $ 2 * (abs (P.indexOffPtr field i) + de)

-- diagonalEnergyContribution ::

shuffleVector :: (Prim i, PrimMonad m, RandomGen g) => Int -> Ptr i -> g -> m g
shuffleVector !n !v !g₀ = go g₀ (n - 1)
  where
    swap !i !j = do
      a <- P.readOffPtr v i
      b <- P.readOffPtr v j
      P.writeOffPtr v i b
      P.writeOffPtr v j a
    go !g !j
      | j > 0 = do
          let (i, g') = uniformWord32R' (fromIntegral j) g
          swap (fromIntegral i) j
          go g' (j - 1)
      | otherwise = return g
{-# SCC shuffleVector #-}

generateAcceptanceProbabilities ::
  (Prim a, RealFloat a, UniformRange a, RandomGen g, PrimMonad m) => a -> Int -> Ptr a -> g -> m g
generateAcceptanceProbabilities β n probabilities g₀ = go g₀ 0
  where
    !pre = -1 / β
    go !g !i
      | i < n = do
          let (!u, !g') = uniformR (0, 1) g
          P.writeOffPtr probabilities i $ pre * (log u)
          go g' (i + 1)
      | otherwise = return g
{-# SCC generateAcceptanceProbabilities #-}

divMod64 :: Int -> (# Int, Int #)
divMod64 x = (# x `unsafeShiftR` 6, x .&. 63 #)
{-# INLINE divMod64 #-}

-- indexBits :: RealFloat a => Bits' -> Int -> a
-- indexBits (Bits' p) i = 2 * fromIntegral b - 1
--   where
--     (# block, rest #) = divMod64 i
--     !w = P.indexOffPtr p block
--     !b = (w `unsafeShiftR` rest) .&. 1
-- {-# INLINE indexBits #-}

readBits :: (RealFloat a, PrimMonad m) => Bits' -> Int -> m a
readBits (Bits' p) i = do
  w <- P.readOffPtr p block
  let b = (w `unsafeShiftR` rest) .&. 1
  pure (2 * fromIntegral b - 1)
  where
    (# block, rest #) = divMod64 i
{-# INLINE readBits #-}

-- fold' :: a -> (a -> Bool) -> (a -> a) -> (b -> a -> b) -> b -> b
-- fold' start cond inc combine = go start
--   where
--     go !x !acc
--       | cond x = go (inc x) (combine acc x)
--       | otherwise = acc
-- {-# INLINE fold' #-}

innerProductM :: (Prim a, RealFloat a, PrimMonad m) => Int -> Ptr a -> Bits' -> m a
innerProductM n xs ys = fold1 0 (< n) (+ 1) combine 0
  where
    combine !acc !i = fmaM acc (P.readOffPtr xs i) (readBits ys i)
{-# INLINE innerProductM #-}

matrixVectorElementM :: (Prim i, Integral i, Prim a, RealFloat a, PrimMonad m) => CSR' i a -> Bits' -> Int -> m a
matrixVectorElementM (CSR' _ elts colIdxs rowIdxs) bits i = fold1 b (< e) (+ 1) combine 0
  where
    b = fromIntegral (P.indexOffPtr rowIdxs i)
    e = fromIntegral (P.indexOffPtr rowIdxs (i + 1))
    combine !acc !k = do
      let !j = fromIntegral (P.indexOffPtr colIdxs k)
          !x = P.indexOffPtr elts k
      y <- readBits bits j
      pure $ acc + x * y
{-# SPECIALIZE matrixVectorElementM :: CSR' Int32 Float -> Bits' -> Int -> IO Float #-}
{-# SPECIALIZE matrixVectorElementM :: CSR' Int32 Double -> Bits' -> Int -> IO Double #-}

fmaM :: (Num a, Applicative m) => a -> m a -> m a -> m a
fmaM a b c = fmap (a +) $ (*) <$> b <*> c
{-# INLINE fmaM #-}

computeEnergyM :: (Prim i, Integral i, Prim a, RealFloat a, PrimMonad m) => Hamiltonian' i a -> Bits' -> m a
computeEnergyM (Hamiltonian' matrix@(CSR' n _ _ _) field) bits = (+) <$> matrixPart <*> fieldPart
  where
    fieldPart = innerProductM n field bits
    matrixPart = fold1 0 (< n) (+ 1) combine 0
    combine !acc !i = fmaM acc (readBits bits i) (matrixVectorElementM matrix bits i)
{-# SPECIALIZE computeEnergyM :: Hamiltonian' Int32 Float -> Bits' -> IO Float #-}
{-# SPECIALIZE computeEnergyM :: Hamiltonian' Int32 Double -> Bits' -> IO Double #-}

energyChangeUponFlipM ::
  (Prim i, Integral i, Prim a, RealFloat a, PrimMonad m) => Hamiltonian' i a -> Bits' -> Int -> m a
energyChangeUponFlipM (Hamiltonian' matrix field) bits i = do
  s <- readBits bits i
  matrixPart <- offDiagMatVec
  pure $ -s * (4 * matrixPart + 2 * P.indexOffPtr field i)
  where
    offDiagMatVec = csrRowFoldM matrix (fromIntegral i) combine 0
    combine !acc !j !x
      | fromIntegral i /= j = (acc +) . (x *) <$> readBits bits (fromIntegral j)
      | otherwise = pure acc
{-# INLINE energyChangeUponFlipM #-}
{-# SPECIALIZE energyChangeUponFlipM :: Hamiltonian' Int32 Float -> Bits' -> Int -> IO Float #-}
{-# SPECIALIZE energyChangeUponFlipM :: Hamiltonian' Int32 Double -> Bits' -> Int -> IO Double #-}

computeEnergyChanges ::
  (Prim i, Integral i, Prim a, RealFloat a, PrimMonad m) => Hamiltonian' i a -> Bits' -> Ptr a -> m ()
computeEnergyChanges h@(Hamiltonian' (CSR' n _ _ _) _) bits energyChanges =
  loop1 0 (< n) (+ 1) $ \i ->
    P.writeOffPtr energyChanges i =<< energyChangeUponFlipM h bits i
{-# SPECIALIZE computeEnergyChanges :: Hamiltonian' Int32 Float -> Bits' -> Ptr Float -> IO () #-}
{-# SPECIALIZE computeEnergyChanges :: Hamiltonian' Int32 Double -> Bits' -> Ptr Double -> IO () #-}

flipBit :: PrimMonad m => Bits' -> Int -> m ()
flipBit (Bits' bits) i = do
  s <- P.readOffPtr bits iBlock
  P.writeOffPtr bits iBlock $ s `xor` (1 `unsafeShiftL` iRest)
  where
    (# iBlock, iRest #) = divMod64 i
{-# INLINE flipBit #-}

flipAndUpdateEnergyChanges' ::
  (Prim i, Integral i, Prim a, RealFloat a, PrimMonad m) => CSR' i a -> Bits' -> Ptr a -> Int -> m ()
flipAndUpdateEnergyChanges' (CSR' _ elts colIdxs rowIdxs) (Bits' bits) energyChanges i = do
  let b = fromIntegral $ P.indexOffPtr rowIdxs i
      e = fromIntegral $ P.indexOffPtr rowIdxs (i + 1)
      (# iBlock, iRest #) = divMod64 i
  s <- P.readOffPtr bits iBlock
  let s' = s `xor` (1 `unsafeShiftL` iRest)
      !pre = -8 * (2 * fromIntegral ((s' `unsafeShiftR` iRest) .&. 1) - 1)
  P.writeOffPtr bits iBlock s'
  P.writeOffPtr energyChanges i . negate =<< P.readOffPtr energyChanges i
  -- computeEnergyChanges' h (Bits' bits) energyChanges
  loop1 b (< e) (+ 1) $ \k -> do
    let j = fromIntegral (P.indexOffPtr colIdxs k)
    when (j /= i) $ do
      sigma <- readBits (Bits' bits) j
      de <- P.readOffPtr energyChanges j
      P.writeOffPtr energyChanges j (de + pre * sigma * P.indexOffPtr elts k)
{-# SCC flipAndUpdateEnergyChanges' #-}
{-# SPECIALIZE flipAndUpdateEnergyChanges' :: CSR' Int32 Double -> Bits' -> Ptr Double -> Int -> IO () #-}

compensatedPlus :: Num a => a -> a -> a -> (# a, a #)
compensatedPlus t c de = (# t', c' #)
  where
    de' = de + c
    t' = t + de'
    c' = de' - (t' - t)
{-# INLINE compensatedPlus #-}

copyBits :: PrimMonad m => Int -> Bits' -> Bits' -> m ()
copyBits count (Bits' target) (Bits' source) =
  P.copyPtr target source ((count + 63) `div` 64)
{-# INLINE copyBits #-}

runStep ::
  (Prim i, Integral i, PrimMonad m) => MutableState i Double -> Int -> Float -> m ()
runStep (MutableState (Hamiltonian' matrix@(CSR' n _ _ _) _) bits energyChanges best@(Bits' bestPtr) acc) i p = do
  de <- P.readOffPtr energyChanges i
  when (realToFrac de < p) $ do
    flipAndUpdateEnergyChanges' matrix bits energyChanges i
    t <- P.readOffPtr acc 0
    c <- P.readOffPtr acc 1
    let (# t', c' #) = compensatedPlus t c de
    P.writeOffPtr acc 0 t'
    P.writeOffPtr acc 1 c'
    eBest <- P.readOffPtr acc 2
    when (t' < eBest) $ do
      P.writeOffPtr acc 2 t'
      when (bestPtr /= P.nullPtr) $ copyBits n best bits
{-# INLINE runStep #-}

runSweeps ::
  (Int -> Float) ->
  Int ->
  MutableState Int32 Double ->
  Ptr Int32 ->
  Ptr Float ->
  CongruentialState ->
  IO CongruentialState
runSweeps schedule numberSweeps s order probs =
  fold1 0 (< numberSweeps) (+ 1) runSweep
  where
    (MutableState (Hamiltonian' (CSR' n _ _ _) _) _ _ _ _) = s
    runSweep !g !stepIdx = do
      g1 <- shuffleVector n order g
      g2 <- generateAcceptanceProbabilities (schedule stepIdx) n probs g1
      loop1 0 (< n) (+ 1) $ \k -> do
        i <- P.readOffPtr order k
        p <- P.readOffPtr probs k
        runStep s (fromIntegral i) p
      pure g2
{-# SCC runSweeps #-}

anneal' ::
  (Int -> Float) ->
  Int ->
  Hamiltonian' Int32 Double ->
  Bits' ->
  CongruentialState ->
  IO (Double, CongruentialState)
anneal' schedule numberSweeps h@(Hamiltonian' (CSR' n _ _ _) _) bits g = do
  energyChanges <- SM.replicate n 0
  best <- SM.replicate ((n + 64) `div` 64) 0
  acc <- SM.replicate 3 0
  order <- SM.generate n fromIntegral
  probs <- SM.replicate n 0
  SM.unsafeWith energyChanges $ \energyChangesPtr ->
    SM.unsafeWith best $ \bestPtr ->
      SM.unsafeWith acc $ \accPtr ->
        SM.unsafeWith order $ \orderPtr ->
          SM.unsafeWith probs $ \probsPtr -> do
            computeEnergyChanges h bits energyChangesPtr
            copyBits n (Bits' bestPtr) bits
            e <- computeEnergyM h bits
            P.writeOffPtr accPtr 0 e
            P.writeOffPtr accPtr 1 0
            P.writeOffPtr accPtr 2 e
            let s = MutableState h bits energyChangesPtr (Bits' bestPtr) accPtr
            g' <- runSweeps schedule numberSweeps s orderPtr probsPtr g
            copyBits n bits (Bits' bestPtr)
            e' <- P.readOffPtr accPtr 2
            pure (e', g')
{-# NOINLINE anneal' #-}
{-# SCC anneal' #-}

annealParallel' ::
  (Int -> Float) ->
  Int ->
  Hamiltonian' Int32 Double ->
  [(Bits', CongruentialState)] ->
  IO [(Double, CongruentialState)]
annealParallel' schedule numberSweeps hamiltonian = pooledMapConcurrently go
  where
    go (bits, g) = do
      (e, g') <- anneal' schedule numberSweeps hamiltonian bits g
      e `deepseq` g' `deepseq` pure (e, g')

optimizeLocally ::
  (Prim i, Integral i, Storable a, Prim a, RealFloat a) => Hamiltonian' i a -> Bits' -> IO Int
optimizeLocally h@(Hamiltonian' matrix@(CSR' n _ _ _) _) bits = do
  energyChanges <- SM.replicate n 0
  SM.unsafeWith energyChanges $ \energyChangesPtr -> do
    computeEnergyChanges h bits energyChangesPtr
    let combineM !didWork !i = do
          de <- P.readOffPtr energyChangesPtr i
          if de < 0
            then do
              flipAndUpdateEnergyChanges' matrix bits energyChangesPtr i
              pure True
            else pure didWork
        go !numberSweeps = do
          didWork <- fold1 0 (< n) (+ 1) combineM False
          if didWork
            then go (numberSweeps + 1)
            else pure numberSweeps
    go 0
{-# SPECIALIZE optimizeLocally :: Hamiltonian' Int32 Double -> Bits' -> IO Int #-}

data GreedySolveState i a
  = GreedySolveState
      {-# UNPACK #-} !(Hamiltonian' i a)
      -- ^ Hamiltonian
      {-# UNPACK #-} !Bits'
      -- ^ Spin configuration
      {-# UNPACK #-} !(Clusters i a)
      -- ^ Clusters
      {-# UNPACK #-} !(Ptr Int)
      -- ^ Tuple of numberClusters and nextClusterIndex

newtype Clusters i a = Clusters (Ptr Int)

getCluster :: (Integral i, PrimMonad m) => Clusters i a -> i -> m Int
getCluster (Clusters p) i = P.readOffPtr p (fromIntegral i)
{-# INLINE getCluster #-}

setCluster :: (Integral i, PrimMonad m) => Clusters i a -> i -> Int -> m ()
setCluster (Clusters p) i = P.writeOffPtr p (fromIntegral i)
{-# INLINE setCluster #-}

mergeClusters ::
  (Integral i, RealFloat a, PrimMonad m) =>
  GreedySolveState i a ->
  i ->
  Int ->
  i ->
  Int ->
  a ->
  m ()
mergeClusters (GreedySolveState (Hamiltonian' (CSR' n _ _ _) _) bits clusters acc) !s1 !cluster1 !s2 !cluster2 !coupling = do
  sign1 <- readBits bits (fromIntegral s1)
  sign2 <- readBits bits (fromIntegral s2)
  let !isFrustrated = sign1 * sign2 * coupling > 0
  loop1 0 (< fromIntegral n) (+ 1) $ \i -> do
    cluster <- getCluster clusters i
    when (cluster == cluster2) $ do
      when isFrustrated $ flipBit bits (fromIntegral i)
      setCluster clusters i cluster1
  -- Decrease the number of clusters by one
  P.writeOffPtr acc 0 . (\k -> k - 1) =<< P.readOffPtr acc 0

addToCluster ::
  (Prim i, Integral i, Prim a, RealFloat a, PrimMonad m) =>
  GreedySolveState i a ->
  Int ->
  i ->
  m ()
addToCluster (GreedySolveState (Hamiltonian' matrix field) bits clusters _) !cluster !s = do
  cluster1 <- getCluster clusters s
  when (cluster1 /= -1) $ error "ouch 3"

  de <- energyChangeM
  when (de < 0) $ flipBit bits (fromIntegral s)
  setCluster clusters s cluster
  where
    combine !acc !j !x = do
      clusterj <- getCluster clusters j
      if clusterj == cluster
        then (acc +) . (x *) <$> readBits bits (fromIntegral j)
        else pure acc
    energyChangeM = do
      sign <- readBits bits (fromIntegral s)
      matrixPart <- csrRowFoldM matrix s combine 0
      pure $ -sign * (2 * matrixPart) --  + P.indexOffPtr field (fromIntegral s))
{-# INLINE addToCluster #-}

createNewCluster ::
  (Integral i, RealFloat a, PrimMonad m) =>
  GreedySolveState i a ->
  i ->
  i ->
  a ->
  m ()
createNewCluster (GreedySolveState _ bits clusters acc) !s1 !s2 !coupling = do
  cluster1 <- getCluster clusters s1
  when (cluster1 /= -1) $ error "ouch 1"
  cluster2 <- getCluster clusters s2
  when (cluster2 /= -1) $ error "ouch 2"
  -- sign1 <- readBits bits (fromIntegral s1)
  -- (sign2 :: Float) <- readBits bits (fromIntegral s2)
  -- when (sign1 /= sign2) $ error "ooops"
  when (coupling > 0) $ flipBit bits (fromIntegral s2)
  -- Increase the number of clusters by one
  P.writeOffPtr acc 0 . (+ 1) =<< P.readOffPtr acc 0
  nextClusterIndex <- P.readOffPtr acc 1
  setCluster clusters s1 nextClusterIndex
  setCluster clusters s2 nextClusterIndex
  P.writeOffPtr acc 1 (nextClusterIndex + 1)

processCoupling ::
  (Prim i, Integral i, Prim a, RealFloat a, PrimMonad m) =>
  GreedySolveState i a ->
  i ->
  i ->
  a ->
  m ()
processCoupling state@(GreedySolveState _ _ clusters _) !s1 !s2 !c
  | s1 /= s2 = do
      cluster1 <- getCluster clusters s1
      cluster2 <- getCluster clusters s2
      let inClusters1 = cluster1 /= -1
          inClusters2 = cluster2 /= -1
      if
          | inClusters1 && inClusters2 && cluster1 == cluster2 -> pure ()
          | inClusters1 && inClusters2 -> mergeClusters state s1 cluster1 s2 cluster2 c
          | inClusters1 -> addToCluster state cluster1 s2
          | inClusters2 -> addToCluster state cluster2 s1
          | otherwise -> createNewCluster state s1 s2 c
  | otherwise = pure ()

greedySolve ::
  (Storable i, Prim i, Integral i, Storable a, Prim a, RealFloat a, Show a) =>
  Hamiltonian' i a ->
  Bits' ->
  IO a
greedySolve h@(Hamiltonian' matrix@(CSR' n _ _ _) _) bits = do
  withCOO matrix $ \coo@(COO nnz elts rowIdxs colIdxs) -> do
    order <- largestFirstOrder coo
    clusters <- SM.replicate n (-1)
    acc <- SM.replicate 2 0
    case bits of
      Bits' bitsPtr -> P.setPtr bitsPtr (div (n + 63) 64) 0
    S.unsafeWith order $ \orderPtr ->
      SM.unsafeWith clusters $ \clustersPtr ->
        SM.unsafeWith acc $ \accPtr -> do
          let state = GreedySolveState h bits (Clusters clustersPtr) accPtr
          loop1 0 (< nnz) (+ 1) $ \orderIdx -> do
            let k = P.indexOffPtr orderPtr orderIdx
                s1 = P.indexOffPtr rowIdxs k
                s2 = P.indexOffPtr colIdxs k
                c = P.indexOffPtr elts k
            when (orderIdx < 10) $ print c
            processCoupling state s1 s2 c
          numberClusters <- P.readOffPtr accPtr 0
          when (numberClusters /= 1) . error $ "numberClusters: " <> show numberClusters
    _ <- optimizeLocally h bits
    computeEnergyM h bits

largestFirstOrder ::
  (Prim a, RealFloat a) =>
  COO i a ->
  IO (S.Vector Int)
largestFirstOrder (COO nnz elts _ _) = do
  order <- SM.generate nnz id
  Intro.sortBy comp order
  S.unsafeFreeze order
  where
    comp !i !j = compare (abs (P.indexOffPtr elts j)) (abs (P.indexOffPtr elts i))

withCOO ::
  (Storable i, Prim i, Integral i) =>
  CSR' i a ->
  (COO i a -> IO b) ->
  IO b
withCOO (CSR' n elts colIdxs rowIdxs) f = do
  let nnz = fromIntegral (P.indexOffPtr rowIdxs n)
  cooRowIndices <- SM.new nnz
  SM.unsafeWith cooRowIndices $ \cooRowIndicesPtr -> do
    loop1 0 (< n) (+ 1) $ \i ->
      let b = P.indexOffPtr rowIdxs i
          e = P.indexOffPtr rowIdxs (i + 1)
          startPtr = cooRowIndicesPtr `P.advancePtr` fromIntegral b
          count = fromIntegral (e - b)
       in P.setPtr startPtr count (fromIntegral i)
    f (COO nnz elts cooRowIndicesPtr colIdxs)

fold1 :: Monad m => a -> (a -> Bool) -> (a -> a) -> (b -> a -> m b) -> b -> m b
fold1 start cond inc combine = go start
  where
    go !x !acc
      | cond x = acc `combine` x >>= go (inc x)
      | otherwise = return acc
{-# INLINE fold1 #-}

loop1 :: Monad m => a -> (a -> Bool) -> (a -> a) -> (a -> m ()) -> m ()
loop1 start cond inc f = go start
  where
    go !x
      | cond x = f x >> go (inc x)
      | otherwise = return ()
{-# INLINE loop1 #-}

----------------------------------------------------------------------------------------------------
-- Random number generation
----------------------------------------------------------------------------------------------------

newtype CongruentialState = CongruentialState Word32
  deriving stock (Show)
  deriving newtype (NFData)

instance RandomGen CongruentialState where
  split = error "CongruentialState is not splittable"
  genWord32 (CongruentialState s) =
    let (a :: Word64) = 0xadb4a92d
        (c :: Word64) = 1
        !s' = fromIntegral $ a * fromIntegral s + c
     in (s', CongruentialState s')
  {-# INLINE genWord32 #-}
  {-# SCC genWord32 #-}
  genWord64 g =
    let (w₁, g₁) = genWord32 g
        (w₂, g₂) = genWord32 g₁
     in ((fromIntegral w₁ `shiftL` 32) .|. fromIntegral w₂, g₂)
  {-# INLINE genWord64 #-}
  {-# SCC genWord64 #-}

uniformWord32R' :: forall g. RandomGen g => Word32 -> g -> (Word32, g)
uniformWord32R' !r !g₀ = go g₀
  where
    -- t :: Word32
    -- t = (- r) `mod` r -- Calculates 2^32 `mod` r!!!
    go :: g -> (Word32, g)
    go g =
      let (w, g') = genWord32 g
          (m :: Word64) = fromIntegral r * fromIntegral w
       in (fromIntegral (m `shiftR` 32), g')
