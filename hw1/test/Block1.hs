{-# LANGUAGE RankNTypes #-}

module Block1
  ( test
  ) where

import Data.List (elemIndex)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromJust)
import Hedgehog (Gen, Property, diff, forAll, property, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (Spec, testSpec)

import ADT.Natural (Nat (..))
import ADT.Tree (Tree (..), delete, find, fromList, insert, isEmpty, size)
import ADT.Week (DayOfWeek (..), afterDays, daysToParty, isWeekend, nextDay)

import qualified Util

-- | testing the first block of homework
test:: IO TestTree
test = do
  t1 <- testSpec "Week:" testWeekSpec
  t21 <- testSpec "Natural Specs:" testNatSpec
  let t22 = testNatProperty
  t3 <- testSpec "Tree:" testTreeSpec
  return $ testGroup "Block 1:" [t1, t21, t22, t3]

days = [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]

testWeekSpec :: Spec
testWeekSpec = do
  Util.testS "nextDay test" (map nextDay days) (tail days ++ [head days])
  Util.testS "afterDays test"
             [ afterDays x i | x <- days, i <- [0..1000] ]
             [ days !! ((fromJust (elemIndex x days) + i) `mod` 7)
             | x <- days, i <- [0..1000]]
  Util.testS "isWeekend test"
             (map isWeekend days)
             [False, False, False, False, False, True, True]
  Util.testS "daysToParty test" (map daysToParty days) [4, 3, 2, 1, 0, 6, 5]

rangeList :: [Integer]
rangeList = [0..100]

natFromInteger :: Integer -> Nat
natFromInteger = fromInteger

testNatOperation
  :: String
  -> (forall a. Integral a => a -> a -> a)
  -> (Integer -> Integer)
  -> (Integer -> Integer)
  -> Spec
testNatOperation testName op lower upper =
  Util.testListS testName
                 (\(x, y) -> toInteger (natFromInteger x `op` natFromInteger y))
                 (uncurry op)
                 [(x, y) | x <- rangeList, y <- [(lower x)..(upper x)]]

testNatSpec :: Spec
testNatSpec = do
  Util.testS "fromInteger test"
             (map natFromInteger rangeList)
             (take 101 (iterate S Z))
  Util.testS "toInteger test" (map toInteger (take 101 (iterate S Z))) rangeList
  testNatOperation "sum test"      (+) (const 0) (const 100)
  testNatOperation "subtract test" (-) (const 0) id
  testNatOperation "multiply test" (*) (const 0) (const 100)
  Util.testListS "equivalence test"
                 (\(x, y) -> (natFromInteger x == natFromInteger y,
                              natFromInteger x /= natFromInteger y))
                 (\(x, y) -> (x == y, x /= y))
                 [(x, y) | x <- rangeList, y <- rangeList]
  Util.testS "order test"
    [ ord (natFromInteger x) (natFromInteger y)
    | x <- rangeList, y <- rangeList, ord <- [(<), (>), (<=), (>=)]]
    [ ord x y | x <- rangeList, y <- rangeList, ord <- [(<), (>), (<=), (>=)]]
  Util.testListS "even test" (even . (fromInteger :: Integer -> Nat)) even rangeList
  testNatOperation "div test" div (const 1) (const 100)
  testNatOperation "mod test" mod (const 1) (const 100)

genNat :: Gen Nat
genNat = natFromInteger <$> Util.genIntegerFromTo 0 100

genNatNonZero :: Gen Nat
genNatNonZero = natFromInteger <$> Util.genIntegerFromTo 1 100

testNatProperty :: TestTree
testNatProperty = Util.testProps "Natural Properties:"
    [("fromInteger and toInteger test"   , testNatToFrom          ),
     ("sum associtiavity test"           , testNatSumAssociativity),
     ("sum commutativity test"           , testNatSumCommutativity),
     ("additive identity test"           , testNatAdditiveIdentity),
     ("multiplication associtiavity test", testNatMulAssociativity),
     ("multiplication commutativity test", testNatSumCommutativity),
     ("multiplicative identity test"     , testNatMulIdentity     ),
     ("distributivity test"              , testNatDistributivity  ),
     ("sum and subtraction test"         , testNatSumSub          ),
     ("division laws test"               , testNatDivMod          ),
     ("even test"                        , testNatEven            ),
     ("multiplication and division test" , testNatMulDiv          )]

testNatToFrom :: Property
testNatToFrom = Util.testProp1 (toInteger . natFromInteger) id Util.genInteger

testNatSumAssociativity :: Property
testNatSumAssociativity = Util.testPropAssoc (+) genNat

testNatSumCommutativity :: Property
testNatSumCommutativity = Util.testPropComm (+) genNat

testNatAdditiveIdentity :: Property
testNatAdditiveIdentity = Util.testProp1 (+ natFromInteger 0) id genNat

testNatMulAssociativity :: Property
testNatMulAssociativity = Util.testPropAssoc (*) genNat

testNatMulCommutativity :: Property
testNatMulCommutativity = Util.testPropComm (*) genNat

testNatMulIdentity :: Property
testNatMulIdentity = Util.testProp1 (* natFromInteger 1) id genNat

testNatDistributivity :: Property
testNatDistributivity = Util.testProp3_ (\x y z -> (x + y) * z)
                                        (\x y z -> (x * z) + (y * z))
                                        genNat

testNatSumSub :: Property
testNatSumSub = Util.testProp2_ (\x y -> x + y - y) const genNat

testNatDivMod :: Property
testNatDivMod = property $ do
  x <- forAll genNat
  y <- forAll genNatNonZero
  x === y * (x `div` y) + x `mod` y
  diff (x `mod` y) (<) y

testNatEven :: Property
testNatEven = Util.testProp1 (even . natFromInteger) even Util.genInteger

testNatMulDiv :: Property
testNatMulDiv = Util.testProp2 (\x y -> (x * y `div` y, x * y `mod` y))
                               (\x y -> (x, natFromInteger 0))
                               genNat
                               genNatNonZero

treeList :: [[Int]]
treeList = [[], [1], [1, 1], [1, 2], replicate 10 1, [1..100]]

testTreeSpec :: Spec
testTreeSpec = do
  Util.testS
    "fromList test"
    (map fromList [[], [1], replicate 10 1])
    [Leaf,
     Branch Leaf (1 :| []) Leaf,
     Branch Leaf (1 :| replicate 9 1) Leaf]
  Util.testListS "null test" null (isEmpty . fromList) treeList
  Util.testListS "size test" length (size . fromList) treeList
  Util.testListS "find test"
                 (\(x, l) -> find x (fromList l))
                 (uncurry elem)
                 ((,) <$> [0, 1, 2, 100] <*> treeList)
  Util.testListS "insertion test"
                 (\(x, l) -> size $ insert x l)
                 (\(_, l) -> size l + 1)
                 ((,) <$> [0, 1, 2, 100] <*> (fromList <$> treeList))
  Util.testListS "deletion test"
                 (\(x, l) -> size (delete x (fromList l)))
                 (\(x, l) -> if x `elem` l
                   then size (fromList l) - 1
                   else size (fromList l))
                 ((,) <$> [0, 1, 2, 100] <*> treeList)
