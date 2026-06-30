module InterpreterSpec (spec) where

import AST
import Control.Monad (replicateM)
import qualified Data.Map.Strict as Map
import Interpreter (Env, eval, execProgram, returnVars)
import Test.Hspec

spec :: Spec
spec = do
  describe "eval" $ do
    it "looks up a variable" $
      eval (Map.fromList [("x", IntV 3)]) (Var "x") `shouldBe` Right (IntV 3)

    it "returns Left for unbound variable" $
      eval Map.empty (Var "z") `shouldSatisfy` \r -> case r of Left _ -> True; _ -> False

    it "evaluates integer addition" $
      eval Map.empty (BinOp Add (Lit (IntV 2)) (Lit (IntV 3))) `shouldBe` Right (IntV 5)

    it "evaluates float addition" $
      eval Map.empty (BinOp Add (Lit (RealV 1.5)) (Lit (RealV 0.5))) `shouldBe` Right (RealV 2.0)

    it "evaluates equality" $
      eval Map.empty (BinOp Eq (Lit (IntV 1)) (Lit (IntV 1))) `shouldBe` Right (BoolV True)

    it "evaluates inequality" $
      eval Map.empty (BinOp Eq (Lit (IntV 1)) (Lit (IntV 2))) `shouldBe` Right (BoolV False)

    it "evaluates less-than" $
      eval Map.empty (BinOp Lt (Lit (IntV 1)) (Lit (IntV 2))) `shouldBe` Right (BoolV True)

  describe "execProgram" $ do
    it "deterministic let: x = 42 is in final env" $ do
      let prog = Program [Let "x" (Lit (IntV 42))]
      (env, ok) <- execProgram prog
      Map.lookup "x" env `shouldBe` Just (IntV 42)
      ok `shouldBe` True

    it "observe succeeds when variable matches" $ do
      let prog =
            Program
              [ Let "x" (Lit (IntV 1)),
                Observe "x" (Lit (IntV 1))
              ]
      (_, ok) <- execProgram prog
      ok `shouldBe` True

    it "observe fails when variable does not match" $ do
      let prog =
            Program
              [ Let "x" (Lit (IntV 0)),
                Observe "x" (Lit (IntV 1))
              ]
      (_, ok) <- execProgram prog
      ok `shouldBe` False

    it "Bernoulli(0) always samples 0" $ do
      let prog = Program [Sample "x" (Distribution "Bernoulli" [Lit (RealV 0.0)])]
      results <- replicateM 50 (execProgram prog)
      all (\(env, _) -> Map.lookup "x" env == Just (IntV 0)) results `shouldBe` True

    it "Bernoulli(1) always samples 1" $ do
      let prog = Program [Sample "x" (Distribution "Bernoulli" [Lit (RealV 1.0)])]
      results <- replicateM 50 (execProgram prog)
      all (\(env, _) -> Map.lookup "x" env == Just (IntV 1)) results `shouldBe` True

    it "if-then-else takes correct branch" $ do
      let prog =
            Program
              [ Let "flag" (Lit (BoolV True)),
                If
                  (Var "flag")
                  [Let "result" (Lit (IntV 1))]
                  [Let "result" (Lit (IntV 0))]
              ]
      (env, _) <- execProgram prog
      Map.lookup "result" env `shouldBe` Just (IntV 1)

    it "returnVars extracts variable names from Return statement" $
      returnVars (Program [Return ["x", "y"]]) `shouldBe` ["x", "y"]
