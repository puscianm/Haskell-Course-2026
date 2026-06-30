module DistributionsSpec (spec) where

import Control.Monad (replicateM)
import Distributions
import Test.Hspec
import Test.QuickCheck

empiricalMeanI :: [Int] -> Double
empiricalMeanI xs = fromIntegral (sum xs) / fromIntegral (length xs)

empiricalVarI :: [Int] -> Double
empiricalVarI xs =
  let avg = empiricalMeanI xs
      n = fromIntegral (length xs)
   in sum (map (\x -> (fromIntegral x - avg) ^ (2 :: Int)) xs) / n

-- +-5 sigma  
checkMeanAndVar :: IO Int -> Double -> Double -> Int -> IO Bool
checkMeanAndVar sampler mu var n = do
  xs <- replicateM n sampler
  let mHat = empiricalMeanI xs
      vHat = empiricalVarI xs
      -- SE of sample mean  = sqrt(var/n)
      -- SE of sample var   ~= var * sqrt(2/(n-1))  (for large n)
      tolM = 5 * sqrt (var / fromIntegral n)
      tolV = 5 * var * sqrt (2 / fromIntegral (n - 1))
  return (abs (mHat - mu) < tolM && abs (vHat - var) < tolV)

empiricalMean :: [Bool] -> Double
empiricalMean xs = fromIntegral (length (filter id xs)) / fromIntegral (length xs)

-- for 0/1 values: Var = p - p^2
empiricalVar :: [Bool] -> Double
empiricalVar xs =
  let m = empiricalMean xs
   in m - m * m

empiricalFreqs :: Int -> [Int] -> [Double]
empiricalFreqs k xs =
  [ fromIntegral (length (filter (== i) xs)) / fromIntegral (length xs)
  | i <- [0 .. k - 1]
  ]

categoricalVars :: Int -> [Int] -> [Double]
categoricalVars k xs =
  let freqs = empiricalFreqs k xs
   in map (\f -> f - f * f) freqs

bernoulliMeanClose :: Double -> Int -> IO Bool
bernoulliMeanClose p n = do
  xs <- replicateM n (sampleBernoulli p)
  let m = empiricalMean xs
      tol = 3 * sqrt (p * (1 - p) / fromIntegral n) + 1e-9
  return (abs (m - p) < tol)

bernoulliVarClose :: Double -> Int -> IO Bool
bernoulliVarClose p n = do
  xs <- replicateM n (sampleBernoulli p)
  let vHat = empiricalVar xs
      vTrue = p * (1 - p)
      -- SE of sample variance for Bernoulli: simplified to 3*vTrue/sqrt(n)
      tol = 5 * vTrue * sqrt (2 / fromIntegral n) + 1e-9
  return (abs (vHat - vTrue) < tol)

categoricalFreqsClose :: [Double] -> Int -> IO Bool
categoricalFreqsClose probs n = do
  xs <- replicateM n (sampleCategorical probs)
  let freqs = empiricalFreqs (length probs) xs
      tols = map (\p -> 3 * sqrt (p * (1 - p) / fromIntegral n) + 1e-9) probs
  return $ and (zipWith (<) (zipWith (\f p -> abs (f - p)) freqs probs) tols)

categoricalVarsClose :: [Double] -> Int -> IO Bool
categoricalVarsClose probs n = do
  xs <- replicateM n (sampleCategorical probs)
  let vHats = categoricalVars (length probs) xs
      vTrues = map (\p -> p * (1 - p)) probs
      tols = map (\v -> 5 * v * sqrt (2 / fromIntegral n) + 1e-9) vTrues
  return $ and (zipWith (\d t -> abs d < t) (zipWith (-) vHats vTrues) tols)

prop_bernoulliConverges :: Double -> Property
prop_bernoulliConverges p =
  (p > 0.05 && p < 0.95) ==> ioProperty $ do
    xs <- replicateM 5000 (sampleBernoulli p)
    let m = empiricalMean xs
    return (abs (m - p) < 0.05)

genProbs :: Int -> Gen [Double]
genProbs k = do
  xs <- vectorOf k (choose (0.1, 1.0 :: Double))
  let s = sum xs
  return (map (/ s) xs)

prop_categoricalValidIndex :: Positive (Small Int) -> Property
prop_categoricalValidIndex (Positive (Small k)) =
  k >= 1 ==> ioProperty $ do
    probs <- generate (genProbs k)
    xs <- replicateM 200 (sampleCategorical probs)
    return (all (\x -> x >= 0 && x < k) xs)

prop_categoricalConverges :: Property
prop_categoricalConverges = ioProperty $ do
  probs <- generate (genProbs 4)
  xs <- replicateM 5000 (sampleCategorical probs)
  let freqs = empiricalFreqs 4 xs
      tols = map (\p -> 5 * sqrt (p * (1 - p) / 5000) + 0.01) probs
      diffs = zipWith (\f p -> abs (f - p)) freqs probs
  return (and (zipWith (<) diffs tols))

prop_binomialBounded :: Positive (Small Int) -> Double -> Property
prop_binomialBounded (Positive (Small n)) p =
  (p >= 0 && p <= 1 && n <= 50) ==> ioProperty $ do
    xs <- replicateM 200 (sampleBinomial n p)
    return (all (\x -> x >= 0 && x <= n) xs)

prop_binomialConverges :: Double -> Property
prop_binomialConverges p =
  (p > 0.05 && p < 0.95) ==> ioProperty $ do
    let n = 10
        mu = fromIntegral n * p
        var = mu * (1 - p)
        tol = 5 * sqrt (var / 5000) + 0.01
    xs <- replicateM 5000 (sampleBinomial n p)
    let mHat = empiricalMeanI xs
    return (abs (mHat - mu) < tol)

prop_poissonNonNeg :: Double -> Property
prop_poissonNonNeg lambda =
  (lambda > 0 && lambda < 20) ==> ioProperty $ do
    xs <- replicateM 200 (samplePoisson lambda)
    return (all (>= 0) xs)

prop_poissonConverges :: Double -> Property
prop_poissonConverges lambda =
  (lambda > 0.5 && lambda < 15) ==> ioProperty $ do
    let tol = 5 * sqrt (lambda / 5000) + 0.01
    xs <- replicateM 5000 (samplePoisson lambda)
    let mHat = empiricalMeanI xs
    return (abs (mHat - lambda) < tol)

prop_geometricPositive :: Double -> Property
prop_geometricPositive p =
  (p > 0.05 && p <= 1.0) ==> ioProperty $ do
    xs <- replicateM 200 (sampleGeometric p)
    return (all (>= 1) xs)

prop_geometricConverges :: Double -> Property
prop_geometricConverges p =
  (p > 0.2 && p < 0.95) ==> ioProperty $ do
    let mu = 1.0 / p
        var = (1 - p) / (p * p)
        tol = 5 * sqrt (var / 5000) + 0.05
    xs <- replicateM 5000 (sampleGeometric p)
    let mHat = empiricalMeanI xs
    return (abs (mHat - mu) < tol)

-- NegBinomial: result is always >= r (need r successes, so at least r trials).
prop_negBinomialBounded :: Property
prop_negBinomialBounded = ioProperty $ do
  r <- generate (choose (1, 5 :: Int))
  p <- generate (choose (0.3, 0.9 :: Double))
  xs <- replicateM 200 (sampleNegBinomial r p)
  return (all (>= r) xs)

prop_negBinomialConverges :: Double -> Property
prop_negBinomialConverges p =
  (p > 0.3 && p < 0.9) ==> ioProperty $ do
    let r = 3
        mu = fromIntegral r / p
        var = fromIntegral r * (1 - p) / (p * p)
        tol = 5 * sqrt (var / 5000) + 0.05
    xs <- replicateM 5000 (sampleNegBinomial r p)
    let mHat = empiricalMeanI xs
    return (abs (mHat - mu) < tol)



spec :: Spec
spec = do
  describe "sampleBernoulli" $ do
    it "always returns False for p = 0" $ do
      xs <- replicateM 200 (sampleBernoulli 0.0)
      xs `shouldBe` replicate 200 False

    it "always returns True for p = 1" $ do
      xs <- replicateM 200 (sampleBernoulli 1.0)
      xs `shouldBe` replicate 200 True

    it "mean converges to p = 0.3 (10 000 draws)" $ do
      ok <- bernoulliMeanClose 0.3 10000
      ok `shouldBe` True

    it "mean converges to p = 0.7 (10 000 draws)" $ do
      ok <- bernoulliMeanClose 0.7 10000
      ok `shouldBe` True

    it "mean converges to p = 0.5 (10 000 draws)" $ do
      ok <- bernoulliMeanClose 0.5 10000
      ok `shouldBe` True

    it "convergence holds for random p in (0.05, 0.95) [QuickCheck]" $
      property prop_bernoulliConverges

    it "variance converges to p*(1-p) for p = 0.3 (10 000 draws)" $ do
      ok <- bernoulliVarClose 0.3 10000
      ok `shouldBe` True

    it "variance converges to p*(1-p) for p = 0.5 (10 000 draws)" $ do
      ok <- bernoulliVarClose 0.5 10000
      ok `shouldBe` True

    it "variance converges to p*(1-p) for p = 0.7 (10 000 draws)" $ do
      ok <- bernoulliVarClose 0.7 10000
      ok `shouldBe` True

  describe "sampleCategorical" $ do
    it "always returns 0 for a one-element distribution" $ do
      xs <- replicateM 100 (sampleCategorical [1.0])
      xs `shouldBe` replicate 100 0

    it "always returns 0 when only first category has mass" $ do
      xs <- replicateM 100 (sampleCategorical [1.0, 0.0, 0.0])
      xs `shouldBe` replicate 100 0

    it "always returns 2 when only last category has mass" $ do
      xs <- replicateM 100 (sampleCategorical [0.0, 0.0, 1.0])
      xs `shouldBe` replicate 100 2

    it "frequencies converge for [0.5, 0.3, 0.2] (10 000 draws)" $ do
      ok <- categoricalFreqsClose [0.5, 0.3, 0.2] 10000
      ok `shouldBe` True

    it "frequencies converge for uniform [0.25, 0.25, 0.25, 0.25] (10 000 draws)" $ do
      ok <- categoricalFreqsClose [0.25, 0.25, 0.25, 0.25] 10000
      ok `shouldBe` True

    it "per-category variance converges to p_i*(1-p_i) for [0.5, 0.3, 0.2] (10 000 draws)" $ do
      ok <- categoricalVarsClose [0.5, 0.3, 0.2] 10000
      ok `shouldBe` True

    it "per-category variance converges to p_i*(1-p_i) for uniform [0.25, 0.25, 0.25, 0.25] (10 000 draws)" $ do
      ok <- categoricalVarsClose [0.25, 0.25, 0.25, 0.25] 10000
      ok `shouldBe` True

    it "sampler always returns a valid index for random prob vectors [QuickCheck]" $
      property prop_categoricalValidIndex

    it "frequencies converge for random 4-category prob vectors [QuickCheck]" $
      property prop_categoricalConverges

  describe "sampleBinomial" $ do
    it "Binomial(1, p) behaves like Bernoulli: always 0 for p=0" $ do
      xs <- replicateM 100 (sampleBinomial 1 0.0)
      xs `shouldBe` replicate 100 0

    it "Binomial(1, p) behaves like Bernoulli: always 1 for p=1" $ do
      xs <- replicateM 100 (sampleBinomial 1 1.0)
      xs `shouldBe` replicate 100 1

    it "mean and variance converge for Binomial(20, 0.4) (10 000 draws)" $ do
      -- mean = 20*0.4 = 8,  var = 20*0.4*0.6 = 4.8
      -- ok <- checkMeanAndVar (sampleBinomial 20 0.4) 8.0 4.8 1000
      ok <- checkMeanAndVar (sampleBinomial 20 0.4) 8.0 4.8 10000
      ok `shouldBe` True

    it "mean and variance converge for Binomial(10, 0.7) (10 000 draws)" $ do
      -- mean = 7,  var = 2.1
      ok <- checkMeanAndVar (sampleBinomial 10 0.7) 7.0 2.1 10000
      ok `shouldBe` True

    it "result is always in [0, n] for random (n, p) [QuickCheck]" $
      property prop_binomialBounded

    it "mean converges to n*p for random p [QuickCheck]" $
      property prop_binomialConverges

  describe "samplePoisson" $ do
    it "Poisson(0) always returns 0" $ do
      xs <- replicateM 100 (samplePoisson 0.0)
      xs `shouldBe` replicate 100 0

    it "mean and variance converge for Poisson(3) (10 000 draws)" $ do
      ok <- checkMeanAndVar (samplePoisson 3.0) 3.0 3.0 10000
      ok `shouldBe` True

    it "mean and variance converge for Poisson(10) (10 000 draws)" $ do
      ok <- checkMeanAndVar (samplePoisson 10.0) 10.0 10.0 10000
      ok `shouldBe` True

    it "result is always non-negative for random lambda [QuickCheck]" $
      property prop_poissonNonNeg

    it "mean converges to lambda for random lambda [QuickCheck]" $
      property prop_poissonConverges

  describe "sampleGeometric" $ do
    it "Geometric(1) always returns 1" $ do
      xs <- replicateM 100 (sampleGeometric 1.0)
      xs `shouldBe` replicate 100 1

    it "always returns a positive value" $ do
      xs <- replicateM 200 (sampleGeometric 0.5)
      all (>= 1) xs `shouldBe` True

    it "mean and variance converge for Geometric(0.5) (10 000 draws)" $ do
      -- mean = 2,  var = (0.5)/0.25 = 2
      ok <- checkMeanAndVar (sampleGeometric 0.5) 2.0 2.0 10000
      ok `shouldBe` True

    it "mean and variance converge for Geometric(0.25) (10 000 draws)" $ do
      -- mean = 4,  var = 0.75/0.0625 = 12
      ok <- checkMeanAndVar (sampleGeometric 0.25) 4.0 12.0 10000
      ok `shouldBe` True

    it "result is always >= 1 for random p [QuickCheck]" $
      property prop_geometricPositive

    it "mean converges to 1/p for random p [QuickCheck]" $
      property prop_geometricConverges

  describe "sampleNegBinomial" $ do
    it "NegBinomial(1, p) has the same mean as Geometric(p)" $ do
      -- mean = 1/0.5 = 2
      ok <- checkMeanAndVar (sampleNegBinomial 1 0.5) 2.0 2.0 10000
      ok `shouldBe` True

    it "mean and variance converge for NegBinomial(3, 0.5) (10 000 draws)" $ do
      -- mean = 3/0.5 = 6,  var = 3*0.5/0.25 = 6
      ok <- checkMeanAndVar (sampleNegBinomial 3 0.5) 6.0 6.0 10000
      ok `shouldBe` True

    it "mean and variance converge for NegBinomial(5, 0.25) (10 000 draws)" $ do
      -- mean = 5/0.25 = 20,  var = 5*0.75/0.0625 = 60
      ok <- checkMeanAndVar (sampleNegBinomial 5 0.25) 20.0 60.0 10000
      ok `shouldBe` True

    it "result is always >= r [QuickCheck]" $
      property prop_negBinomialBounded

    it "mean converges to r/p for random p [QuickCheck]" $
      property prop_negBinomialConverges
