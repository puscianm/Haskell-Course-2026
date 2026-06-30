module Main (main) where

import DistributionsSpec
import InferenceSpec
import InterpreterSpec
import ParserSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  DistributionsSpec.spec
  ParserSpec.spec
  InterpreterSpec.spec
  InferenceSpec.spec
