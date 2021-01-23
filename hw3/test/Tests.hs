module Tests
  ( test
  ) where

import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State.Strict (evalState, execState)
import Data.Either (isRight)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (Spec, it, shouldBe, testSpec)

import FileSystem (System, emptySys)
import Interface (FSActions (..), FSException, PathInfo (..))
import Task (MockFS (..))

testS :: (Show a, Eq a) => String -> a -> a -> Spec
testS testName arg1 arg2 = it testName $ shouldBe arg1 arg2

eval :: MockFS a -> System -> Either FSException a
eval = evalState . runExceptT . runMockFS

exec :: MockFS a -> System -> System
exec = execState . runExceptT . runMockFS

bigFS :: System
bigFS = exec mock emptySys where
  mock :: MockFS ()
  mock = do
    touch "foo"
    mkdir "bar"
    touch "bar/foo"
    mkdir "baz"
    mkdir "baz/baz"
    mkdir "baz/baz/baz"

longPath2Baz :: FilePath
longPath2Baz = "/baz/../baz/baz/baz/../.././../././baz"

isDirInfo :: PathInfo -> Bool
isDirInfo FileInfo {}      = False
isDirInfo DirectoryInfo {} = True


-- | Testing of the homework
test:: IO TestTree
test = do
  t1 <- testSpec "stringSum Specs:" testMockSpec
  return $ testGroup "Block 1:" [t1]

testMockSpec :: Spec
testMockSpec = do
  testS
    "pwd"
    (eval pwd bigFS)
    (Right "/")
  testS
    "cd #1" (eval (cd "baz" >> pwd) bigFS) (Right "/baz")
  testS
    "cd #2" (eval (cd longPath2Baz >> pwd) bigFS) (Right "/baz")
  testS
    "cd \"..\"" (isRight $ eval (cd "..") bigFS) False
  testS
    "dir" (eval dir bigFS) (Right ["foo", "baz", "bar"])
  testS
    "ls \"bar\" #1" (eval (ls "bar") bigFS) (Right ["foo"])
  testS
    "ls \"bar\" #2" (eval (ls "/bar") bigFS) (Right ["foo"])
  testS
    "ls \"baz\"" (eval (ls longPath2Baz) bigFS) (Right ["baz"])
  testS
    "ls \"foo\"" (isRight $ eval (ls "foo") bigFS) False
  testS
    "find \".\" \"foo\""
    (eval (find "." "foo") bigFS)
    (Right ["foo", "bar" </> "foo"])
  testS
    "mkdir \"su\""
    (eval (mkdir "su" >> dir) bigFS)
    (Right ["foo", "su", "baz", "bar"])
  testS
    "touch \"su\""
    (eval (touch "su" >> dir) bigFS)
    (Right ["su", "foo", "baz", "bar"])
  testS
    "info \"su\" #1"
    (eval (mkdir "su" >> info "su") bigFS >>= Right . isDirInfo)
    (Right True)
  testS
    "info \"su\" #2"
    (eval (touch "su" >> info "su") bigFS >>= Right . isDirInfo)
    (Right False)
  testS
    "echo/cat \"foo\""
    (eval ( cd "bar" >>
            echo "foo" "мама мыла раму" >>
            cat ("longPath2Baz" </> "../bar/foo")
          ) bigFS)
    (Right "мама мыла раму")
  testS
    "del \"foo\" #1"
    (eval (del "foo" >> dir) bigFS)
    (Right ["baz", "bar"])
  testS
    "del \"bar\" #2"
    (eval (del "bar" >> dir) bigFS)
    (Right ["foo", "baz"])
