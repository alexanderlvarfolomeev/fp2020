module Block2
 ( test
 ) where

import Data.Foldable (toList)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid (Endo (..))
import Hedgehog (Property)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (Spec, testSpec)

import ADT.Tree (Tree (..), find, fromList, size)
import Fold.Split (joinWith, splitOn)
import Fold.Tree ()

import qualified Util

-- | testing the second block of homework
test:: IO TestTree
test = do
  t11 <- testSpec "Tree Specs:" testTreeSpec
  let t12 = testTreeProperty
  t21 <- testSpec "splitOn and joinWith Specs:" testSplitSpec
  let t22 = testSplitProperty
  return $ testGroup "Block 2:" [t11, t12, t21, t22]

lists :: [[Int]]
lists = [[]            ,
         [1]           ,
         [1, 1]        ,
         [1, 2]        ,
         [2, 1]        ,
         replicate 10 1,
         [100, 99..1]  ,
         [1..100]      ]

testTreeSpec :: Spec
testTreeSpec = do
  Util.testListS "toList test"  (toList . fromList)         sort     lists
  Util.testListS "foldMap test" (foldMap (: []) . fromList) sort     lists
  Util.testListS "foldMap test" (foldr (:) [] . fromList)   sort     lists
  Util.testListS "length test"  (length . fromList)         length   lists
  Util.testListS "sum test"     (sum . fromList)            sum      lists
  Util.testListS "elem test"    (elem 1 . fromList)         (elem 1) lists

testTreeProperty :: TestTree
testTreeProperty = Util.testProps "Tree Foldable Properties:"
    [("toList and fromList test", testTreeToFromList),
     ("foldr and foldMap test"  , testTreeFold      ),
     ("sum test"                , testTreeSum       )]

testTreeToFromList :: Property
testTreeToFromList =
  Util.testProp1 (toList . fromList) sort (Util.genIntList (-100) 100)

testTreeFold :: Property
testTreeFold = Util.testProp1 (foldr (+) 0)
                              (\x -> appEndo (foldMap (Endo . (+)) x) 0)
                              (Util.genIntList (-100) 100)

testTreeSum :: Property
testTreeSum = Util.testProp1 (sum . fromList) sum (Util.genIntList (-100) 100)

testSplitSpec :: Spec
testSplitSpec = do
  Util.testS "simple splitOn test"
             (splitOn '/' "path/to/file")
             ("path" :| ["to", "file"])
  Util.testS "empty list splitOn test" (splitOn '/' "") ("" :| [])
  Util.testS "list doesn't contain an element splitOn test"
             (splitOn '.' "path/to/file")
             ("path/to/file" :| [])
  Util.testS "list contains only split elements splitOn test"
             (splitOn '/' $ replicate 10 '/')
             ("" :| replicate 10 "")
  Util.testS "simple joinWith test"
             (joinWith '/' ("path" :| ["to", "file"]))
             "path/to/file"
  Util.testS "empty list joinWith test" (joinWith '/' ("" :| [])) ""
  Util.testS "list doesn't contain an element joinWith test"
             (joinWith '.' ("path/to/file" :| []))
             "path/to/file"
  Util.testS "list contains only split elements joinWith test"
             (joinWith '/' ("" :| replicate 10 ""))
             (replicate 10 '/')


testSplitProperty :: TestTree
testSplitProperty = Util.testProps "splitOn and joinWith Properties:"
    [("splitOn and joinWith round-trip test", testSplitJoinRoundTrip)]

testSplitJoinRoundTrip :: Property
testSplitJoinRoundTrip = Util.testProp2 (\x -> joinWith x . splitOn x)
                                        (flip const)
                                        (Util.genIntFromTo 0 11)
                                        (Util.genIntList 0 10)
