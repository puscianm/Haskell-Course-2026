module Main (main) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Inference
import Interpreter (returnVars)
import Parser (parseProgram)
import Text.Megaparsec (errorBundlePretty)

medicalDiagnosis :: String
medicalDiagnosis =
  unlines
    [ "// Bayesian medical diagnosis",
      "has_disease ~ Bernoulli(0.01);",
      "let tp = 0.95;",
      "let fp = 0.10;",
      "if has_disease then {",
      "  test ~ Bernoulli(tp);",
      "} else {",
      "  test ~ Bernoulli(fp);",
      "}",
      "observe test = 1;",
      "return [has_disease];"
    ]

-- Analytic answer: P(has_disease | test=1)
--   = 0.95 * 0.01 / (0.95 * 0.01 + 0.10 * 0.99)  ~= 0.0876
analyticAnswer :: Double
analyticAnswer = 0.95 * 0.01 / (0.95 * 0.01 + 0.10 * 0.99)

main :: IO ()
main = do
  putStrLn "=== ProbLang: Bayesian Medical Diagnosis ==="
  putStrLn ""

  prog <- case parseProgram medicalDiagnosis of
    Left err -> fail (errorBundlePretty err)
    Right prog -> return prog

  let nSamples = 10000
      maxAttempts = 2000000

  putStrLn $ "Collecting " ++ show nSamples ++ " accepted samples..."
  result <- rejectionSample prog nSamples maxAttempts

  let accepted = length (irSamples result)
      attempts = irAttempts result
      rate = fromIntegral accepted / fromIntegral attempts :: Double
      vars = returnVars prog
      post = posterior (irSamples result) vars

  putStrLn $ "  Total executions : " ++ show attempts
  putStrLn $ "  Accepted traces  : " ++ show accepted
  putStrLn $ "  Acceptance rate  : " ++ showPct rate
  putStrLn ""
  putStrLn "Posterior estimates:"
  mapM_ (printVar post) vars
  putStrLn ""
  putStrLn $ "Analytic answer  : P(has_disease=1 | test=1) = " ++ show analyticAnswer

printVar :: Map String Double -> String -> IO ()
printVar post v =
  case Map.lookup v post of
    Nothing -> putStrLn $ "  " ++ v ++ " : (not found)"
    Just p -> putStrLn $ "  P(" ++ v ++ " = 1) = " ++ show p

showPct :: Double -> String
showPct x = show (fromIntegral (round (x * 10000) :: Int) / 100.0 :: Double) ++ "%"
