module Main
  ( main
  ) where

import Lib (repl)

-- | main for @stack run@
main :: IO ()
main = repl
