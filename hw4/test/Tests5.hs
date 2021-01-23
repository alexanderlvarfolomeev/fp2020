module Tests5
  ( test
  ) where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, it, shouldBe, testSpec)

import Task4 (sample, signSample)
import Task5 (showHSFun1)

-- | Testing of HalyavaScript
test:: IO TestTree
test = testSpec "HalyavaScript Specs:" tests

testS :: (Show a, Eq a) => String -> a -> a -> Spec
testS testName arg1 arg2 = it testName $ shouldBe arg1 arg2

tests :: Spec
tests = do
  testS
    "sample"
    (showHSFun1 sample)
    ( "function(v0) {\n" ++
      "var v1 = 0\n" ++
      "var v2 = 0\n" ++
      "v2 = 1\n" ++
      "v1 = 0\n" ++
      "while ((v0) > (v2)) {\n" ++
      "v2 = (v2) + (v2)\n" ++
      "v1 = (v1) + (1)\n" ++
      "}\n" ++
      "return v1\n" ++
      "}")
  testS
    "signSample"
    (showHSFun1 signSample)
    ( "function(v0) {\n" ++
      "var v1 = \"\"\n" ++
      "v1 = if ((v0) > (0)) {\n" ++
      "\"Positive\"\n" ++
      "} else {\n" ++
      "if ((0) > (v0)) {\n" ++
      "\"Negative\"\n" ++
      "} else {\n" ++
      "\"Zero\"\n" ++
      "}\n" ++
      "}\n" ++
      "return v1\n" ++
      "}")
