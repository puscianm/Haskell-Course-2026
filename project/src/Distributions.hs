module Distributions
  ( sampleBernoulli,
    sampleCategorical,
    sampleBinomial,
    samplePoisson,
    sampleGeometric,
    sampleNegBinomial,
  )
where

import Control.Monad (replicateM)
import System.Random (randomRIO)

sampleBernoulli :: Double -> IO Bool
sampleBernoulli p = do
  r <- randomRIO (0.0, 1.0 :: Double)
  return (r < p)

-- returns 0-based index; probabilities should sum to 1
sampleCategorical :: [Double] -> IO Int
sampleCategorical probs = do
  r <- randomRIO (0.0, 1.0 :: Double)
  return (pick r (zip [0 ..] probs))
  where
    pick _ [] = length probs - 1
    pick r ((i, p) : rest)
      | r < p = i
      | otherwise = pick (r - p) rest

sampleBinomial :: Int -> Double -> IO Int
sampleBinomial n p = do
  xs <- replicateM n (sampleBernoulli p)
  return (length (filter id xs))

-- Knuth's algorithm
samplePoisson :: Double -> IO Int
samplePoisson lambda = go 0 1.0
  where
    threshold = exp (-lambda)
    go k p = do
      u <- randomRIO (0.0, 1.0 :: Double)
      let p' = p * u
      -- putStrLn $ "poisson step k=" ++ show k ++ " p'=" ++ show p'
      if p' > threshold
        then go (k + 1) p'
        else return k

-- inverse CDF: ceil(log(U) / log(1-p))
sampleGeometric :: Double -> IO Int
sampleGeometric p
  | p >= 1.0 = return 1 -- certain success on the first trial
  | otherwise = do
      u <- randomRIO (0.0, 1.0 :: Double)
      let u' = max u 1e-300 -- guard against log(0)
      return (ceiling (log u' / log (1 - p)))

-- sum of r geometric draws
sampleNegBinomial :: Int -> Double -> IO Int
sampleNegBinomial r p = do
  xs <- replicateM r (sampleGeometric p)
  return (sum xs)
