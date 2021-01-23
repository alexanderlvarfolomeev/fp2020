module Main
 ( main
 ) where

import Test.Tasty

import Block1 as B1
import Block2 as B2
import Block3 as B3

-- | testing of the first homework
main :: IO ()
main = do
  b1 <- B1.test
  b2 <- B2.test
  b3 <- B3.test
  defaultMain $ testGroup "HW1:" [b1, b2, b3]
