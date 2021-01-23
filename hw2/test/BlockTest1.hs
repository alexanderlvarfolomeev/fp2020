module BlockTest1
  ( test
  ) where

import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Hedgehog (Gen, Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (Spec, testSpec)

import Block1 (NonEmpty (..), Tree (..), stringSum)
import Utils (genArg, genList, testFoldS, testS)


-- | testing the first block of homework
test:: IO TestTree
test = do
  t11 <- testSpec "stringSum Specs:" testStringSumSpec
  let t12 = testStringSumProperty
  t2 <- testSpec "Tree Specs:" testTreeSpec
  t3 <- testSpec "NonEmpty Specs:" testNonEmptySpec
  return $ testGroup "Block 1:" [t11, t12, t2, t3]

strings :: [String]
strings = [ "0", "1", "-1", "", "\n\r",
            show (maxBound :: Int), show (minBound :: Int),
            "0 0 00    0\n 1 \t\v 0", "           1          ",
            "\n\n\n\n\n\r-100\v\v\v", "1 2 3", "-1 1 -1 1 -1 1",
            show (maxBound :: Int) ++ " " ++ show (minBound :: Int),
            "input", "output", "1, 2, 3", "15 6ny0ne 4ere?",
            "459032jioeg4039kjg43 0", "0F6AD", "--100" ]

maybes :: [Maybe Int]
maybes = [ Just 0, Just 1, Just (-1), Just 0, Just 0,
           Just (maxBound :: Int), Just (minBound :: Int),
           Just 1, Just 1,
           Just (-100), Just 6, Just 0,
           Just (-1),
           Nothing, Nothing, Nothing, Nothing,
           Nothing, Nothing, Nothing ]

testStringSumSpec :: Spec
testStringSumSpec = testS "Several simple unit tests"
                          maybes
                          (map stringSum strings)

genArgList :: Gen [(String, Maybe Int)]
genArgList = genList $ genArg 2000 1

testStringSumProperty :: TestTree
testStringSumProperty = testProperty "stringSum Properties" . property $ do
  xs <- forAll genArgList
  let (strs, mbs) = unzip xs
  let genDelim = forAll $ Gen.element ["\t", "\n", "\r", "\v", " ", "\n\r"]
  str <- foldr (\s gens -> do {s' <- gens; d <- genDelim; pure $ s ++ d ++ s'})
               genDelim
               strs
  stringSum str === (sum <$> sequenceA mbs)


testList :: [Int]
testList = [0..127]

testTree :: Tree Int
testTree = list2Tree testList

testNE :: NonEmpty Int
testNE = list2NonEmpty testList

testFunTree1 :: Tree (Int -> Int)
testFunTree1 = Branch (Leaf (+10000)) (Branch (Leaf (+20000)) (Leaf (+30000)))

testFunTree2 :: Tree (Int -> Int)
testFunTree2 = Branch (Branch (Leaf (*20)) (Leaf (*30))) (Leaf (*10))

testFunNE1 :: NonEmpty (Int -> Int)
testFunNE1 = (+10000) :| [(+20000), (+30000)]

testFunNE2 :: NonEmpty (Int -> Int)
testFunNE2 = (*20) :| [(*30), (*10)]

list2Tree :: [a] -> Tree a
list2Tree xs = let len = length xs
               in case len of
  0 -> undefined
  1 -> Leaf $ head xs
  _ -> let (l, r) = splitAt (len `div` 2) xs in Branch (list2Tree l)
                                                       (list2Tree r)

list2NonEmpty :: [a] -> NonEmpty a
list2NonEmpty [] = error "Empty list"
list2NonEmpty (x : rest) = x :| rest

testTraversableSpec :: (Traversable t,
                        Applicative t,
                        Show (t Int),
                        Eq (t Int),
                        Show (t (Maybe Int)),
                        Eq (t (Maybe Int)))
                        => t Int -> t (Int -> Int) -> t (Int -> Int) -> Spec
testTraversableSpec tr testFuns1 testFuns2 = do
  testS "Functor identity law" tr (fmap id tr)
  testS "Functor composition law"
            (fmap ((*10) . (+10000)) tr)
            (fmap (*10) . fmap (+10000) $ tr)
  testS "Applicative identity law"  tr (pure id <*> tr)
  testS "Applicative composition law"
            (pure (.) <*> testFuns1 <*> testFuns2 <*> tr)
            (testFuns1 <*> (testFuns2 <*> tr))
  testS "Applicative homomorphism law"
        (pure (+10000) <*> pure 1)
        (head [pure 10001, tr])
  testS "Applicative interchange law"
        (testFuns1 <*> pure 1)
        (pure ($ 1) <*> testFuns1)
  testS "Foldable sum test" (sum tr) (sum testList)
  testS "Foldable length test" (length tr) (length testList)
  testS "Foldable elem test #1" (elem 1 tr) (elem 1 testList)
  testS "Foldable elem test #2" (elem 1024 tr) (elem 1024 testList)
  testS "Traversable naturality law"
            (fmap (fmap (*10)) . sequenceA $ (Just <$> tr))
            (sequenceA . fmap (fmap (*10)) $ Just <$> tr)
  testS "Traversable identity law"
            (sequenceA $ fmap (Identity . Just) tr)
            (Identity (Just <$> tr))
  testS "Traversable composition law"
        (sequenceA (Compose . Just . Just <$> tr))
        (Compose . fmap sequenceA $ sequenceA (Just . Just <$> tr))

testTreeSpec :: Spec
testTreeSpec = testTraversableSpec testTree testFunTree1 testFunTree2

testNonEmptySpec :: Spec
testNonEmptySpec = testTraversableSpec testNE testFunNE1 testFunNE2
