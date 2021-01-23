module Main
 ( main
 ) where

import Test.Tasty (testGroup, defaultMain)

import BlockTest1 as B1
import BlockTest2 as B2
import BlockTest3 as B3

main :: IO ()
main = do
  b1 <- B1.test
  b2 <- B2.test
  b3 <- B3.test
  defaultMain $ testGroup "HW2:" [b1, b2, b3]
