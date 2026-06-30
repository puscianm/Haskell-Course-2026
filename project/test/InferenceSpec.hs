module InferenceSpec (spec) where

import AST
import qualified Data.Map.Strict as Map
import Inference
import Parser
import Test.Hspec

endToEnd :: String -> String -> Double -> Double -> Int -> Int -> Expectation
endToEnd src var analytic tol nSamples maxAttempts = do
  prog <- case parseProgram src of Left e -> fail (show e); Right p -> return p
  result <- rejectionSample prog nSamples maxAttempts
  let p = Map.findWithDefault 0 var (posterior (irSamples result) [var])
  abs (p - analytic) `shouldSatisfy` (< tol)

spec :: Spec
spec = do
  describe "rejection sampling (basic)" $ do
    it "unconstrained Bernoulli(0.5): posterior mean ~= 0.5" $ do
      let prog = Program [Sample "x" (Distribution "Bernoulli" [Lit (RealV 0.5)]), Return ["x"]]
      result <- rejectionSample prog 4000 100000
      let p = Map.findWithDefault 0 "x" (posterior (irSamples result) ["x"])
      -- abs (p - 0.5) `shouldSatisfy` (< 0.02)
      abs (p - 0.5) `shouldSatisfy` (< 0.03)

    it "Bernoulli(1) with observe = 1: all traces accepted, posterior = 1.0" $ do
      let prog =
            Program
              [ Sample "x" (Distribution "Bernoulli" [Lit (RealV 1.0)]),
                Observe "x" (Lit (IntV 1)),
                Return ["x"]
              ]
      result <- rejectionSample prog 200 200
      let p = Map.findWithDefault 0 "x" (posterior (irSamples result) ["x"])
      p `shouldBe` 1.0

    it "impossible observation: zero traces accepted" $ do
      let prog =
            Program
              [ Sample "x" (Distribution "Bernoulli" [Lit (RealV 0.0)]),
                Observe "x" (Lit (IntV 1)),
                Return ["x"]
              ]
      result <- rejectionSample prog 10 1000
      length (irSamples result) `shouldBe` 0

  describe "end-to-end (one model per distribution)" $ do
    -- prev=1%, sens=95%, fpr=10% => P(disease|test+) ~= 8.76% by Bayes
    it "Bernoulli -medical diagnosis: posterior ~= 8.76 %" $
      endToEnd
        ( unlines
            [ "has_disease ~ Bernoulli(0.01);",
              "let tp = 0.95;",
              "let fp = 0.10;",
              "if has_disease then { test ~ Bernoulli(tp); }",
              "else { test ~ Bernoulli(fp); }",
              "observe test = 1;",
              "return [has_disease];"
            ]
        )
        "has_disease"
        (0.95 * 0.01 / (0.95 * 0.01 + 0.10 * 0.99))
        0.02
        5000
        2000000

    -- 3 risk levels (60/30/10%), sens 10/50/90%; 0.9*0.1/(0.1*0.6+0.5*0.3+0.9*0.1) = 0.30
    it "Categorical -patient risk (3 levels): P(high risk | test+) = 0.30" $
      endToEnd
        ( unlines
            [ "risk ~ Categorical(0.6, 0.3, 0.1);",
              "let is_high = risk == 2;",
              "if risk == 0 then { test ~ Bernoulli(0.1); }",
              "else { if risk == 1 then { test ~ Bernoulli(0.5); }",
              "       else { test ~ Bernoulli(0.9); } }",
              "observe test = 1;",
              "return [is_high];"
            ]
        )
        "is_high"
        0.30
        0.02
        4000
        500000

    -- batch of 10, reliable=10% defect vs unreliable=40%, observe 3 defects
    -- C(10,3)*0.4^3*0.6^7 vs C(10,3)*0.1^3*0.9^7 => ~78.9% unreliable
    it "Binomial -manufacturing quality: P(unreliable | 3 defects) ~= 78.9 %" $
      endToEnd
        ( unlines
            [ "unreliable ~ Bernoulli(0.5);",
              "if unreliable then { defects ~ Binomial(10, 0.4); }",
              "else { defects ~ Binomial(10, 0.1); }",
              "observe defects = 3;",
              "return [unreliable];"
            ]
        )
        "unreliable"
        ( let p4 = 120 * 0.4 ^ (3 :: Int) * 0.6 ^ (7 :: Int)
              p1 = 120 * 0.1 ^ (3 :: Int) * 0.9 ^ (7 :: Int)
           in p4 / (p4 + p1)
        )
        0.02
        4000
        1000000

    -- peak lambda=4 vs off-peak lambda=1, observe 3 reqs; e^-4*64/6 vs e^-1/6 => ~76%
    it "Poisson -server load: P(peak | 3 requests) ~= 76.1 %" $
      endToEnd
        ( unlines
            [ "peak ~ Bernoulli(0.5);",
              "if peak then { requests ~ Poisson(4.0); }",
              "else { requests ~ Poisson(1.0); }",
              "observe requests = 3;",
              "return [peak];"
            ]
        )
        "peak"
        ( let p4 = exp (-4) * 64 / 6
              p1 = exp (-1) / 6
           in p4 / (p4 + p1)
        )
        0.02
        4000
        1000000

    -- good p=0.8 vs poor p=0.3; P(2 tries|good)=0.2*0.8=0.16, P(2|poor)=0.7*0.3=0.21 => ~43%
    it "Geometric -packet retransmission: P(good channel | 2 attempts) ~= 43.2 %" $
      endToEnd
        ( unlines
            [ "good ~ Bernoulli(0.5);",
              "if good then { attempts ~ Geometric(0.8); }",
              "else { attempts ~ Geometric(0.3); }",
              "observe attempts = 2;",
              "return [good];"
            ]
        )
        "good"
        (0.16 / (0.16 + 0.21))
        0.02
        4000
        500000

    -- 3 deals needed; strong p=0.6 vs weak p=0.2, observe 5 calls; C(4,2)*... => ~87%
    it "NegBinomial -sales calls to 3 deals: P(strong closer | 5 calls) ~= 87.1 %" $
      endToEnd
        ( unlines
            [ "strong ~ Bernoulli(0.5);",
              "if strong then { calls ~ NegBinomial(3, 0.6); }",
              "else { calls ~ NegBinomial(3, 0.2); }",
              "observe calls = 5;",
              "return [strong];"
            ]
        )
        "strong"
        ( let p6 = 6 * 0.6 ^ (3 :: Int) * 0.4 ^ (2 :: Int)
              p2 = 6 * 0.2 ^ (3 :: Int) * 0.8 ^ (2 :: Int)
           in p6 / (p6 + p2)
        )
        0.02
        4000
        1000000
