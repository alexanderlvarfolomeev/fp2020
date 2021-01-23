module Util
  ( testSemigroupAssociativityS,
    testListS,
    testS,

    genInt,
    genInteger,
    genIntegerFromTo,
    genIntFromTo,
    genIntList,

    testPropAssoc,
    testPropComm,
    testProps,
    testProp1,
    testProp2,
    testProp2_,
    testProp3,
    testProp3_
  ) where

import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (Spec, it, shouldBe)

-- | Spec test on equality of two arguments
testS :: (Show a, Eq a) => String -> a -> a -> Spec
testS testName arg1 arg2 = it testName $ shouldBe arg1 arg2

-- | '@'testListS' @testName f g list@ is Spec test
-- on equality of function @f@ and @g@ on arguments
-- in @list@
testListS :: Eq b => String -> (a -> b) -> (a -> b) -> [a] -> Spec
testListS testName f g list = testS testName (all (\x -> f x == g x) list) True

-- | Spec test of associativity law of @Semigroup@ instance
testSemigroupAssociativityS
  :: (Semigroup s, Show a, Eq a)
  => String
  -> (Int -> [s])
  -> (s -> a)
  -> Spec
testSemigroupAssociativityS testName intToList finalizer =
  testListS testName
            (\(x, y, z) -> finalizer (x <> (y <> z)))
            (\(x, y, z) -> finalizer ((x <> y) <> z))
            ((,,) <$> intToList 0 <*> intToList 1 <*> intToList 2)

-- | returns @Int@ generator with given range
genIntFromTo :: Int -> Int -> Gen Int
genIntFromTo lower upper = Gen.int $ Range.linear lower upper

-- | returns @Int@ generator with range @[0..10000]@
genInt :: Gen Int
genInt = genIntFromTo 0 10000

-- | returns @Integer@ generator with given range
genIntegerFromTo :: Integer -> Integer -> Gen Integer
genIntegerFromTo lower upper = Gen.integral $ Range.linear lower upper

-- | returns @Integer@ generator with range @[0..10000]@
genInteger :: Gen Integer
genInteger = genIntegerFromTo 0 10000

-- | returns @[Int]@ generator with given range
genIntList :: Int -> Int -> Gen [Int]
genIntList lower upper =
  Gen.list (Range.linear 0 10000) (genIntFromTo lower upper)

-- | tests the list of Property tests
testProps :: String -> [(String, Property)] -> TestTree
testProps groupName = testGroup groupName . map (uncurry testProperty)

-- | Property test of equality of two functions of one argument
testProp1 :: (Show a, Show b, Eq b) => (a -> b) -> (a -> b) -> Gen a -> Property
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

-- | Property test of equality of two functions of two arguments of same type
testProp2_
  :: (Show a, Show b, Eq b)
  => (a -> a -> b)
  -> (a -> a -> b)
  -> Gen a
  -> Property
testProp2_ f g gen = property $ do
  x <- forAll gen
  y <- forAll gen
  f x y === g x y

-- | Property test of equality of two functions of three arguments
testProp3
  :: (Show a1, Show a2, Show a3, Show b, Eq b)
  => (a1 -> a2 -> a3 -> b)
  -> (a1 -> a2 -> a3 -> b)
  -> Gen a1
  -> Gen a2
  -> Gen a3
  -> Property
testProp3 f g gen1 gen2 gen3 = property $ do
  x <- forAll gen1
  y <- forAll gen2
  z <- forAll gen3
  f x y z === g x y z

-- | Property test of equality of two functions of three arguments of same type
testProp3_
  :: (Show a, Show b, Eq b)
  => (a -> a -> a -> b)
  -> (a -> a -> a -> b)
  -> Gen a
  -> Property
testProp3_ f g gen = property $ do
  x <- forAll gen
  y <- forAll gen
  z <- forAll gen
  f x y z === g x y z

-- | Property test of associativity law
testPropAssoc :: (Show a, Eq a) => (a -> a -> a) -> Gen a -> Property
testPropAssoc op = testProp3_ (\x y z -> (x `op` y) `op` z)
                              (\x y z -> x `op` (y `op` z))

-- | Property test of commutativity law
testPropComm :: (Show a, Eq a) => (a -> a -> a) -> Gen a -> Property
testPropComm op = testProp2_ op (flip op)
