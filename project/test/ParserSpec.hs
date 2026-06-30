module ParserSpec (spec) where

import AST
import Parser
import Test.Hspec
import Text.Megaparsec (errorBundlePretty)

shouldParseTo :: String -> Program -> Expectation
shouldParseTo src expected =
  case parseProgram src of
    Left err -> expectationFailure (errorBundlePretty err)
    Right ast -> ast `shouldBe` expected

shouldFailToParse :: String -> Expectation
shouldFailToParse src =
  case parseProgram src of
    Left _ -> return ()
    Right ast -> expectationFailure $ "expected parse failure but got: " ++ show ast

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

spec :: Spec
spec = describe "parseProgram" $ do
  it "parses an empty program" $
    "" `shouldParseTo` Program []

  it "parses a sample statement" $
    "x ~ Bernoulli(0.5);"
      `shouldParseTo` Program [Sample "x" (Distribution "Bernoulli" [Lit (RealV 0.5)])]

  it "parses a let statement with integer literal" $
    "let n = 42;"
      `shouldParseTo` Program [Let "n" (Lit (IntV 42))]

  it "parses a let statement with float literal" $
    "let p = 0.95;"
      `shouldParseTo` Program [Let "p" (Lit (RealV 0.95))]

  it "parses an observe statement" $
    "observe test = 1;"
      `shouldParseTo` Program [Observe "test" (Lit (IntV 1))]

  it "parses a return statement" $
    "return [x, y];"
      `shouldParseTo` Program [Return ["x", "y"]]

  it "parses a binary expression" $
    "let z = a + b;"
      `shouldParseTo` Program [Let "z" (BinOp Add (Var "a") (Var "b"))]

  it "parses an if-else statement" $
    unlines
      [ "if flag then { x ~ Bernoulli(0.9); }",
        "else    { x ~ Bernoulli(0.1); }"
      ]
      `shouldParseTo` Program
        [ If
            (Var "flag")
            [Sample "x" (Distribution "Bernoulli" [Lit (RealV 0.9)])]
            [Sample "x" (Distribution "Bernoulli" [Lit (RealV 0.1)])]
        ]

  it "parses // line comments" $
    "// ignore me\nlet x = 1;" `shouldParseTo` Program [Let "x" (Lit (IntV 1))]

  it "parses the full medical-diagnosis model" $ do
    case parseProgram medicalDiagnosis of
      Left err -> expectationFailure (errorBundlePretty err)
      Right (Program stmts) -> do
        length stmts `shouldBe` 6
        head stmts `shouldBe` Sample "has_disease" (Distribution "Bernoulli" [Lit (RealV 0.01)])
        last stmts `shouldBe` Return ["has_disease"]

  it "rejects a sample statement without semicolon" $
    shouldFailToParse "x ~ Bernoulli(0.5)"

  it "rejects a keyword used as an identifier" $
    shouldFailToParse "let if = 1;"
