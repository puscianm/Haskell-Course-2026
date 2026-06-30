module Inference
  ( InferenceResult (..),
    rejectionSample,
    posterior,
  )
where

import AST
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Interpreter

data InferenceResult = InferenceResult
  { irSamples :: [Env], -- accepted environments
    irAttempts :: Int -- total executions run
  }
  deriving (Show)

rejectionSample :: Program -> Int -> Int -> IO InferenceResult
rejectionSample prog nSamples maxAttempts = go nSamples maxAttempts []
  where
    go 0 attempts acc =
      let result = InferenceResult (reverse acc) (maxAttempts - attempts)
       in return result
    go _ 0 acc = return $ InferenceResult (reverse acc) maxAttempts
    go n attempts acc = do
      (env, ok) <- execProgram prog
      if ok
        then go (n - 1) (attempts - 1) (env : acc)
        else go n (attempts - 1) acc

-- empirical mean across accepted traces= P(var=1) for binary vars
posterior :: [Env] -> [String] -> Map String Double
posterior envs vars =
  Map.fromList [(v, mean (mapMaybe (fmap getValue . Map.lookup v) envs)) | v <- vars]
  where
    getValue (IntV n) = fromIntegral n
    getValue (BoolV b) = if b then 1.0 else 0.0
    getValue (RealV r) = r
    mean [] = 0.0
    mean xs = sum xs / fromIntegral (length xs)
