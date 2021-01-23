module Block3
  ( test
  ) where

import Data.Monoid (Product (..), Sum (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (Spec, testSpec)

import Monoid.Concat (eitherConcat, maybeConcat)
import Monoid.Semigroup (Endo (..), Name (..), NonEmpty (..), ThisOrThat (..))

import qualified Util

-- | testing the third block of homework
test:: IO TestTree
test = do
  t1 <- testSpec "maybeConcat and eitherConcat:" testConcat
  t2 <- testSpec "Semigroups:" testSemigroup
  return $ testGroup "Block 3:" [t1, t2]

testConcat :: Spec
testConcat = do
  Util.testS "maybeConcat test"
             (maybeConcat [Just [1, 2, 3], Nothing, Just [4, 5]])
             [1, 2, 3, 4, 5]
  Util.testS "eitherConcat test"
             (eitherConcat [Left (Sum 3)  ,
                           Right [1, 2, 3],
                           Left (Sum 5)   ,
                           Right [4, 5]   ])
             (Sum 8, [1, 2, 3, 4, 5])
  Util.testS "eitherConcat test 2"
             (eitherConcat ([] :: [Either (Product Integer) (Maybe String)]))
             (Product 1, Nothing)

nonEmptyList :: Int -> [NonEmpty Int]
nonEmptyList i = [i :| [], i :| [i], i :| [i, i]]

thisOrThatList :: Int -> [ThisOrThat Int Int]
thisOrThatList i = [This i, That i, Both i i]

nameList :: Int -> [Name]
nameList i = [Empty                      ,
              Name ""                    ,
              Name $ show i              ,
              Name $ show i ++ show i    ,
              Name $ show i ++ "."       ,
              Name $ "." ++ show i       ,
              Name $ "." ++ show i ++ ".",
              Name ".."                  ]

endoList :: Int -> [Endo Int]
endoList i = Endo id : [Endo (\a -> a * 10 + 3 * i + x) | x <- [1..3]]

testSemigroup :: Spec
testSemigroup = do
  Util.testSemigroupAssociativityS "NonEmpty semigroup associativity test"
                                   nonEmptyList
                                   id
  Util.testSemigroupAssociativityS "ThisOrThat semigroup associativity test"
                                   thisOrThatList
                                   id
  Util.testS "Name test" (Name "root" <> Name "server") (Name "root.server")
  Util.testSemigroupAssociativityS "Name semigroup associativity test"
                                   nameList
                                   id
  Util.testSemigroupAssociativityS "Endo semigroup associativity test"
                                   endoList
                                   (($ 0) . getEndo)
  Util.testListS "Name monoid right identity test" (<> mempty) id (nameList 1)
  Util.testListS "Name monoid left identity test"  (mempty <>) id (nameList 1)
  Util.testListS "Endo monoid right identity test"
                 (($ 0) . getEndo . (<> mempty))
                 (($ 0) . getEndo)
                 (endoList 1)
  Util.testListS "Endo monoid left identity test"
                 (($ 0) . getEndo . (mempty <>))
                 (($ 0) . getEndo)
                 (endoList 1)
