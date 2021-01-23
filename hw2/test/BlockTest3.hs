{-# LANGUAGE TupleSections #-}

module BlockTest3
  ( test
  ) where

import Control.Applicative (liftA2, (<|>))
import Data.Function (on)
import Data.List (uncons, isPrefixOf, drop)
import Data.Maybe (listToMaybe, isJust)
import Text.Read (readMaybe)
import Hedgehog (Gen, Property)
import qualified Hedgehog.Gen as Gen
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (Spec, testSpec)

import Block3 ( Parser (..),
                cbs,
                element,
                eof,
                integerParser,
                listlistParser,
                ok,
                satisfy,
                stream )
import Utils ( genArg,
               genIntFromTo,
               genIntList,
               genList,
               genShortIntList,
               testFoldS,
               testProp1,
               testProp2,
               testProps,
               testS )


-- | testing the third block of homework
test:: IO TestTree
test = do
  t11 <- testSpec "Parser instances Specs:" testInstanceSpec
  t21 <- testSpec "Base combinator Specs:" testBaseCombSpec
  let t22 = testBaseCombProperty
  t31 <- testSpec "Simple parsers Specs:" testSimpleParserSpec
  let t32 = testSimpleParserProperty
  t41 <- testSpec "listlistParser Specs:" testComplexParserSpec
  let t42 = testComplexParserProperty
  return $ testGroup "Block 3:" [t11, t21, t22, t31, t32, t41, t42]

lists :: [[Int]]
lists = [[], [1], [-1], [1, 2], [1, 2, 3], [1..100]]

predicates :: [Int -> Bool]
predicates = [(== 0), (== 1), (< 0), (> 0), const True, const False]

testParser :: Parser Int Int
testParser = Parser uncons

testFun1 :: Parser Int (Int -> Int)
testFun1 = Parser $ \xs -> do
  (_, rest) <- uncons xs
  return ((*10), rest)

testFun2 :: Parser Int (Int -> Int)
testFun2 = Parser $ \xs -> do
  (_, rest) <- uncons xs
  return ((+1000), rest)

byIndex :: Int -> [a] -> Maybe (a, [a])
byIndex ind l = if length l <= ind || ind < 0
                then Nothing
                else let res = l !! ind in Just (res, res : tail l)

testInstanceSpec :: Spec
testInstanceSpec = do
  testFoldS "Functor identity law"
            uncons
            (runParser $ fmap id testParser)
            lists
  testFoldS "Functor composition law"
            (runParser . fmap ((*10) . (+10000)) $ testParser)
            (runParser . fmap (*10) . fmap (+10000) $ testParser)
            lists
  testFoldS "Applicative identity law"
            uncons
            (runParser $ pure id <*> testParser)
            lists
  testFoldS "Applicative composition law"
            (runParser $ pure (.) <*> testFun1 <*> testFun2 <*> testParser)
            (runParser $ testFun1 <*> (testFun2 <*> testParser))
            lists
  testFoldS "Applicative homomorphism law"
            (runParser $ pure (+10000) <*> pure 1)
            (runParser $ pure 10001)
            lists
  testFoldS "Applicative interchange law"
            (runParser $ testFun1 <*> pure 1)
            (runParser $ pure ($ 1) <*> testFun1)
            lists
  testFoldS "Alternative test #1"
            uncons
            (runParser $ Parser (const Nothing) <|> Parser uncons)
            lists
  testFoldS "Alternative test #2"
            uncons
            (runParser $ Parser uncons <|> Parser (const Nothing))
            lists
  testFoldS "Monad left identity law"
            (runParser $ return 0 >>= Parser . byIndex)
            (runParser $ Parser (byIndex 0))
            lists
  testFoldS "Monad right identity law"
            (runParser $ testParser >>= return)
            (runParser testParser)
            lists
  testFoldS "Monad associativity law"
            (runParser $ testParser >>=
              (\x -> Parser (byIndex x) >>= (Parser . byIndex . (+1))))
            (runParser $ (testParser >>= Parser . byIndex) >>=
              Parser . byIndex . (+1))
            lists

testBaseCombSpec :: Spec
testBaseCombSpec = do
  testFoldS "ok test"
            (Just . ("check", ))
            (runParser $ ok "check")
            lists
  testFoldS "eof test"
            (\l -> if null l then Just ((), []) else Nothing)
            (runParser eof)
            lists
  testFoldS "satisfy test"
            (\(p, l) -> listToMaybe l >>= \x -> if p x
                                                then Just (x, tail l)
                                                else Nothing)
            (uncurry $ runParser . satisfy)
            (liftA2 (,) predicates lists)
  testFoldS "element test"
            (\(e, l) -> listToMaybe l >>= \x -> if e == x
                                                then Just (x, tail l)
                                                else Nothing)
            (uncurry $ runParser . element)
            (liftA2 (,) [(-1)..2] lists)
  testFoldS "stream test"
            (\(sub, l) -> if sub `isPrefixOf` l
                          then Just (sub, drop (length sub) l)
                          else Nothing)
            (uncurry $ runParser . stream)
            (liftA2 (,) [[], [1], [1, 2], [1..3]] lists)

testBaseCombProperty :: TestTree
testBaseCombProperty = testProps "Base compinators property test"
                                 [ ("ok test", testOkProperty),
                                   ("eof test", testEofProperty),
                                   ("satisfy test", testSatisfyProperty),
                                   ("element test", testElementProperty),
                                   ("stream test", testStreamProperty) ]

genInts :: Gen [Int]
genInts = genIntList (-100) 100

testOkProperty :: Property
testOkProperty = testProp1 (Just . ("check", ))
                           (runParser $ ok "check")
                           genInts

testEofProperty :: Property
testEofProperty = testProp1 (\l -> if null l then Just ((), []) else Nothing)
                            (runParser eof)
                            genInts

testSatisfyProperty :: Property
testSatisfyProperty =
  testProp2 (\e l -> listToMaybe l >>= \x -> if x > e
                                             then Just (x, tail l)
                                             else Nothing)
            (\e -> runParser $ satisfy (> e))
            (genIntFromTo (-10) 10)
            genInts

testElementProperty :: Property
testElementProperty =
  testProp2 (\e l -> listToMaybe l >>= \x -> if x == e
                                             then Just (x, tail l)
                                             else Nothing)
            (runParser . element)
            (genIntFromTo 0 3)
            (genIntList 0 3)

testStreamProperty :: Property
testStreamProperty = testProp2 (\sub l -> if sub `isPrefixOf` l
                                          then Just (sub, drop (length sub) l)
                                          else Nothing)
                               (runParser . stream)
                               (genShortIntList 0 3)
                               (genIntList 0 3)

isCBS :: String -> Bool
isCBS = helper 0 where
  helper n [] = n == 0
  helper n (x : rest)
    | n >= 0 && x == '(' = helper (n + 1) rest
    | n >= 0 && x == ')' = helper (n - 1) rest
    | otherwise          = False

readInt :: String -> Maybe Int
readInt ('+' : rest) = let crest = if listToMaybe rest == Just '-'
                                   then "-"
                                   else rest in readMaybe crest
readInt s = readMaybe s

bs :: [String]
bs = [ "(", ")", "()", "not a correct bracket sequence", "",
       "((((()))))", "()()", "((())())", "())(", ")))(((", "(x", "x)" ]

ints :: [String]
ints = [ "++100", "-100", "+100", "100", "000000000", "0",
         "1", "+-100", "AABB", "a7428", "NaN" ]

testSimpleParserSpec :: Spec
testSimpleParserSpec = do
  testFoldS "cbs test" isCBS (isJust . runParser cbs) bs
  testFoldS "int parser test" readInt (fmap fst . runParser integerParser) ints

testSimpleParserProperty :: TestTree
testSimpleParserProperty =
  testProps "Simple parsers property test"
            [ ("cbs test", testCBSProperty),
              ("int parser test", testIntParserProperty) ]

testCBSProperty :: Property
testCBSProperty = testProp1 isCBS
                            (isJust . runParser cbs)
                            (genList (Gen.element "()"))

testIntParserProperty :: Property
testIntParserProperty = testProp1 readInt
                                  (fmap fst . runParser integerParser)
                                  (fst <$> genArg 2 1)


listlistTests :: [(String, [[Int]])]
listlistTests = [ ("2, 1,+10  , 3,5,-7, 2", [[1, 10], [5, -7, 2]]),
                  ("", []),
                  ("000000000", [[]]),
                  ("0, 0", [[], []]) ]

testComplexParserSpec :: Spec
testComplexParserSpec = do
  testFoldS "listlistParser test"
            (Just . snd)
            (fmap fst . runParser listlistParser . fst)
            listlistTests

listlist2String :: [[Int]] -> String
listlist2String [] = ""
listlist2String (x : rest) =
  list2String x ++ foldr (\l acc -> ',' : (list2String l ++ acc)) "" rest where
    list2String x = show (length x) ++ foldMap ((',' :) . show) x

genListList :: Gen [[Int]]
genListList = genList (genIntList (-100) 100)

testComplexParserProperty :: TestTree
testComplexParserProperty = testProperty "listlistParser property test" $
  testProp1 Just
            (fmap fst .  runParser listlistParser . listlist2String)
            genListList
