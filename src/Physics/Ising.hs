{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Physics.Ising
  ( -- * Annealing
    Hamiltonian' (..),
    CSR' (..),
    Bits' (..),
    anneal',
    annealParallel',
    computeEnergy',

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
import qualified Data.Vector.Storable.Mutable as MV
import Data.Word (Word32, Word64)
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
  Int ->
  (b -> i -> i -> a -> m b) ->
  b ->
  m b
csrRowFoldM (CSR' _ elts colIdxs rowIdxs) i f = fold1 b (< e) (+ 1) combine
  where
    b = fromIntegral $ P.indexOffPtr rowIdxs i
    e = fromIntegral $ P.indexOffPtr rowIdxs (i + 1)
    combine !acc !k = do !acc' <- f acc (fromIntegral i) j x; pure acc'
      where
        j = P.indexOffPtr colIdxs k
        x = P.indexOffPtr elts k

csrRowFold ::
  (Prim i, Integral i, Prim a) => CSR' i a -> Int -> (b -> i -> i -> a -> b) -> b -> b
csrRowFold matrix i0 f = runStrictIdentity . csrRowFoldM matrix i0 f'
  where
    f' z i j x = let !z' = f z i j x in pure z'

csrFold ::
  (Prim i, Integral i, Prim a, Monad m) =>
  CSR' i a ->
  (b -> i -> i -> a -> m b) ->
  b ->
  m b
csrFold matrix@(CSR' n _ _ _) f = fold1 0 (< n) (+ 1) combine
  where
    combine !acc !k = do !acc' <- csrRowFoldM matrix k f acc; pure acc'

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
          de <- csrRowFoldM m i (\ !z _ _ x -> pure $ min z (abs x)) (1 / 0)
          pure $ min acc $ 2 * (abs (P.indexOffPtr field i) + de)
    !upper = fold1 0 (< n) (+ 1) combine 0
      where
        combine !acc i = do
          de <- csrRowFoldM m i (\ !z _ _ x -> pure $ max z (abs x)) 0
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

indexBits :: RealFloat a => Bits' -> Int -> a
indexBits (Bits' p) i = 2 * fromIntegral b - 1
  where
    (# block, rest #) = divMod64 i
    !w = P.indexOffPtr p block
    !b = (w `unsafeShiftR` rest) .&. 1
{-# INLINE indexBits #-}

readBits :: (RealFloat a, PrimMonad m) => Bits' -> Int -> m a
readBits (Bits' p) i = do
  w <- P.readOffPtr p block
  let b = (w `unsafeShiftR` rest) .&. 1
  pure (2 * fromIntegral b - 1)
  where
    (# block, rest #) = divMod64 i
{-# INLINE readBits #-}

fold' :: a -> (a -> Bool) -> (a -> a) -> (b -> a -> b) -> b -> b
fold' start cond inc combine = go start
  where
    go !x !acc
      | cond x = go (inc x) (combine acc x)
      | otherwise = acc
{-# INLINE fold' #-}

innerProduct :: (Prim a, RealFloat a) => Int -> Ptr a -> Bits' -> a
innerProduct n xs ys = fold' 0 (< n) (+ 1) combine 0
  where
    combine !acc !i = acc + P.indexOffPtr xs i * indexBits ys i
{-# INLINE innerProduct #-}

matrixVectorElement :: (Prim i, Integral i, Prim a, RealFloat a) => CSR' i a -> Bits' -> Int -> a
matrixVectorElement (CSR' _ elts colIdxs rowIdxs) bits i = fold' b (< e) (+ 1) combine 0
  where
    b = fromIntegral (P.indexOffPtr rowIdxs i)
    e = fromIntegral (P.indexOffPtr rowIdxs (i + 1))
    combine !acc !k =
      let !j = fromIntegral (P.indexOffPtr colIdxs k)
       in acc + P.indexOffPtr elts k * indexBits bits j
{-# SPECIALIZE matrixVectorElement :: CSR' Int32 Float -> Bits' -> Int -> Float #-}
{-# SPECIALIZE matrixVectorElement :: CSR' Int32 Double -> Bits' -> Int -> Double #-}

computeEnergy' :: (Prim i, Integral i, Prim a, RealFloat a) => Hamiltonian' i a -> Bits' -> a
computeEnergy' (Hamiltonian' matrix@(CSR' n _ _ _) field) bits = matrixPart + fieldPart
  where
    fieldPart = innerProduct n field bits
    matrixPart = fold' 0 (< n) (+ 1) combine 0
    combine !acc !i = acc + indexBits bits i * matrixVectorElement matrix bits i
{-# SPECIALIZE computeEnergy' :: Hamiltonian' Int32 Float -> Bits' -> Float #-}
{-# SPECIALIZE computeEnergy' :: Hamiltonian' Int32 Double -> Bits' -> Double #-}

computeEnergyChanges' ::
  (Prim i, Integral i, Prim a, RealFloat a, PrimMonad m) => Hamiltonian' i a -> Bits' -> Ptr a -> m ()
computeEnergyChanges' (Hamiltonian' matrix@(CSR' n _ _ _) field) bits energyChanges =
  loop1 0 (< n) (+ 1) $ \i ->
    P.writeOffPtr energyChanges i (changeUponFlip i)
  where
    changeUponFlip i = -indexBits bits i * (4 * offDiagMatVec i + 2 * P.indexOffPtr field i)
    offDiagMatVec i' = csrRowFold matrix i' combine 0
      where
        combine !acc !i !j !x
          | i /= j = acc + x * indexBits bits (fromIntegral j)
          | otherwise = acc
{-# SPECIALIZE computeEnergyChanges' :: Hamiltonian' Int32 Float -> Bits' -> Ptr Float -> IO () #-}
{-# SPECIALIZE computeEnergyChanges' :: Hamiltonian' Int32 Double -> Bits' -> Ptr Double -> IO () #-}

flipAndUpdateEnergyChanges' ::
  (Prim i, Integral i, PrimMonad m) => CSR' i Double -> Bits' -> Ptr Double -> Int -> m ()
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
runStep (MutableState (Hamiltonian' matrix@(CSR' n _ _ _) _) bits energyChanges best acc) i p = do
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
      copyBits n best bits
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
  energyChanges <- MV.replicate n 0
  best <- MV.replicate ((n + 64) `div` 64) 0
  acc <- MV.replicate 3 0
  order <- MV.generate n fromIntegral
  probs <- MV.replicate n 0
  MV.unsafeWith energyChanges $ \energyChangesPtr ->
    MV.unsafeWith best $ \bestPtr ->
      MV.unsafeWith acc $ \accPtr ->
        MV.unsafeWith order $ \orderPtr ->
          MV.unsafeWith probs $ \probsPtr -> do
            computeEnergyChanges' h bits energyChangesPtr
            copyBits n (Bits' bestPtr) bits
            let e = computeEnergy' h bits
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

fold1 :: Monad m => a -> (a -> Bool) -> (a -> a) -> (b -> a -> m b) -> b -> m b
fold1 start cond inc combine init = go start init
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
