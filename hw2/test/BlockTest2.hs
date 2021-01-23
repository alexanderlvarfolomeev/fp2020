module BlockTest2
  ( test
  ) where

import Control.Applicative (liftA2)
import Data.Function (on)
import Data.Tuple.Extra (both)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (Spec, testSpec)

import Block2 (ArithmeticError (..), Expr (..), eval, moving)
import Utils (testS, testFoldS)


-- | testing the second block of homework
test:: IO TestTree
test = do
  t1 <- testSpec "eval Specs:" testEvalSpec
  t2 <- testSpec "moving Specs:" testMovingSpec
  return $ testGroup "Block 2:" [t1, t2]

intList :: [Int]
intList = [(-100)..100]

pairList :: [(Int, Int)]
pairList = liftA2 (,) intList intList

testEvalSpec :: Spec
testEvalSpec = do
  testFoldS "Constant test" Right (eval . Con) intList
  testFoldS "Add test"
            (Right . uncurry (+))
            (eval . uncurry Add . both Con)
            pairList
  testFoldS "Subtract test"
            (Right . uncurry (-))
            (eval . uncurry Subtract . both Con)
            pairList
  testFoldS "Multiply test"
            (Right . uncurry (*))
            (eval . uncurry Multiply . both Con)
            pairList
  testFoldS "Divide correct test"
            (Right . uncurry div)
            (eval . uncurry Divide . both Con)
            (liftA2 (,) intList $ [(-100)..(-1)] ++ [1..100])
  testFoldS "Divide exception test"
            (const $ Left DivisionByZero)
            (eval . flip (on Divide Con) 0)
            intList
  testFoldS "Pow correct test"
            (Right . uncurry (^))
            (eval . uncurry Pow . both Con)
            (liftA2 (,) intList [0..10])
  testFoldS "Pow exception test"
            (const $ Left NegativeExponent)
            (eval . uncurry Pow . both Con)
            (liftA2 (,) intList [(-10)..(-1)])


testMovingSpec :: Spec
testMovingSpec = do
  testS "Simple test #1"
        [1.0, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5]
        (moving 4 [1, 5, 3, 8, 7, 9, 6])
  testS "Simple test #2"
        [1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5]
        (moving 2 [1, 5, 3, 8, 7, 9, 6])
  testS "Simple test #3"
        [1, 5, 3, 8, 7, 9, 6]
        (moving 1 [1, 5, 3, 8, 7, 9, 6])
  testS "Empty input test #1" [] (moving 1 [])
  testS "Empty input test #2" [] (moving 10 [])
