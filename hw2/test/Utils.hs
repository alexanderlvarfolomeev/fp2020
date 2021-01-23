{-# LANGUAGE TupleSections #-}

module Utils
  ( genArg,
    genIntFromTo,
    genIntList,
    genList,
    genShortIntList,

    testFoldS,
    testS,

    testProps,
    testProp1,
    testProp2,
  ) where

import Data.Tuple.Extra ((&&&))
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (Spec, it, shouldBe)


-- | Spec test on equality of two arguments
testS :: (Show a, Eq a) => String -> a -> a -> Spec
testS testName arg1 arg2 = it testName $ shouldBe arg1 arg2

-- | '@'testFoldS' @testName f g fbl@ is Spec test
-- on equality of function @f@ and @g@ on arguments
-- in @fbl@
testFoldS
  :: (Eq b, Foldable f) => String -> (a -> b) -> (a -> b) -> f a -> Spec
testFoldS testName f g fbl = testS testName (all (\x -> f x == g x) fbl) True

-- | returns @Int@ generator with given range
genIntFromTo :: Int -> Int -> Gen Int
genIntFromTo lower upper = Gen.int $ Range.linear lower upper

-- | returns @(String, Maybe Int)@ generator
-- which generates either @Int@ string representation
-- or some rubbish with certain frequency
genArg :: Int -> Int -> Gen (String, Maybe Int)
genArg right left =
  Gen.frequency
    [ (right, (show &&& Just) <$> genIntFromTo (-1000) 1000),
      (left, (, Nothing) <$> Gen.string (Range.linear 2 4) Gen.alpha) ]

-- | returns @[a]@ generator with given range and given @a@ generator
genList :: Gen a -> Gen [a]
genList = Gen.list (Range.linear 100 500)

-- | returns @[Int]@ generator with given range
genIntList :: Int -> Int -> Gen [Int]
genIntList lower upper = genList (genIntFromTo lower upper)

-- | returns @[Int]@ generator with given range and small length
genShortIntList :: Int -> Int -> Gen [Int]
genShortIntList lower upper =
  Gen.list (Range.linear 0 3) (genIntFromTo lower upper)

-- | tests the list of Property tests
testProps :: String -> [(String, Property)] -> TestTree
testProps groupName = testGroup groupName . map (uncurry testProperty)

-- | Property test of equality of two functions of one argument
testProp1
  :: (Show a, Show b, Eq b) => (a -> b) -> (a -> b) -> Gen a -> Property
testProp1 f g gen = property $ do
  x <- forAll gen
  f x === g x

-- | Property test of equality of two functions of two arguments
testProp2
  :: (Show a1, Show a2, Show b, Eq b)
  => (a1 -> a2 -> b)
  -> (a1 -> a2 -> b)
  -> Gen a1
  -> Gen a2
  -> Property
testProp2 f g gen1 gen2 = property $ do
  x <- forAll gen1
  y <- forAll gen2
  f x y === g x y
