module Interpreter
  ( Env,
    execProgram,
    returnVars,
    eval,
  )
where

import AST
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Distributions

type Env = Map String Value

eval :: Env -> Expr -> Either String Value
eval env (Var x) = maybe (Left $ "undefined variable: " ++ x) Right (Map.lookup x env)
eval _ (Lit v) = Right v
eval env (BinOp op l r) = do vl <- eval env l; vr <- eval env r; applyOp op vl vr
eval env (If' cond t f) = do vc <- eval env cond; if toBool vc then eval env t else eval env f

toBool :: Value -> Bool
toBool (BoolV b) = b
toBool (IntV n) = n /= 0
toBool (RealV r) = r /= 0.0

toDouble :: Value -> Either String Double
toDouble (RealV r) = Right r
toDouble (IntV n) = Right (fromIntegral n)
toDouble v = Left $ "expected a number, got: " ++ show v

applyOp :: Op -> Value -> Value -> Either String Value
applyOp op v1 v2 = case op of
  Add -> numOp (+) (+) v1 v2
  Sub -> numOp (-) (-) v1 v2
  Mul -> numOp (*) (*) v1 v2
  Div -> do
    d <- toDouble v2
    n <- toDouble v1
    if d == 0 then Left "division by zero" else Right (RealV (n / d))
  Eq -> Right (BoolV (v1 == v2))
  Lt -> BoolV <$> ((<) <$> toDouble v1 <*> toDouble v2)
  And -> Right (BoolV (toBool v1 && toBool v2))
  Or -> Right (BoolV (toBool v1 || toBool v2))

numOp :: (Int -> Int -> Int) -> (Double -> Double -> Double) -> Value -> Value -> Either String Value
numOp fi _ (IntV a) (IntV b) = Right (IntV (fi a b))
numOp _ fd v1 v2 = RealV <$> (fd <$> toDouble v1 <*> toDouble v2)

sampleDist :: Distribution -> Env -> IO Value
sampleDist (Distribution name paramExprs) env = do
  params <- either (ioError . userError) return (mapM (eval env) paramExprs)
  case (name, params) of
    ("Bernoulli", [p]) -> do
      pv <- err (toDouble p)
      b <- sampleBernoulli pv
      return (IntV (if b then 1 else 0))
    ("Categorical", ps) -> do
      pvs <- err (mapM toDouble ps)
      IntV <$> sampleCategorical pvs
    ("Binomial", [n, p]) -> do
      nv <- err (toDouble n)
      pv <- err (toDouble p)
      IntV <$> sampleBinomial (round nv) pv
    ("Poisson", [lambda]) -> do
      lv <- err (toDouble lambda)
      IntV <$> samplePoisson lv
    ("Geometric", [p]) -> do
      pv <- err (toDouble p)
      IntV <$> sampleGeometric pv
    ("NegBinomial", [r, p]) -> do
      rv <- err (toDouble r)
      pv <- err (toDouble p)
      IntV <$> sampleNegBinomial (round rv) pv
    _ -> ioError . userError $ "unknown distribution: " ++ name
  where
    err = either (ioError . userError) return

runStmts :: Env -> Bool -> [Statement] -> IO (Env, Bool)
runStmts env ok [] = return (env, ok)
runStmts env ok (stmt : rest) = case stmt of
  Sample var dist -> do
    v <- sampleDist dist env
    -- putStrLn $ "sampled " ++ var ++ " = " ++ show v
    runStmts (Map.insert var v env) ok rest
  Let var e -> case eval env e of
    Left err -> ioError (userError err)
    Right v -> runStmts (Map.insert var v env) ok rest
  Observe var e ->
    let satisfied = case (Map.lookup var env, eval env e) of
          (Just actual, Right expected) -> actual == expected
          _ -> False
     in runStmts env (ok && satisfied) rest
  If cond thenBranch elseBranch -> case eval env cond of
    Left err -> ioError (userError err)
    Right v -> do
      let branch = if toBool v then thenBranch else elseBranch
      (env', ok') <- runStmts env ok branch
      runStmts env' ok' rest
  Return _ -> runStmts env ok rest

execProgram :: Program -> IO (Env, Bool)
execProgram (Program stmts) = runStmts Map.empty True stmts

returnVars :: Program -> [String]
returnVars (Program stmts) = concat [vars | Return vars <- stmts]
