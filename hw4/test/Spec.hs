module Main
 ( main
 ) where

import Test.Tasty (defaultMain, testGroup)

import Tests5 (test)

-- | main for @stack test@
main :: IO ()
main = test >>= defaultMain . testGroup "HW4:" . (: [])
