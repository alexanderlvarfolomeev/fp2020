module Lib
    ( main
    ) where

import System.IO (hFlush, stdout)
import Control.Monad (forM_)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Task8 (oneDiseased, showModel, step)

-- | Comonad-19 visualization
main :: IO ()
main = do
  putStr "Write round amount of Comonad-2019: "
  _ <- hFlush stdout
  rounds <- readLn :: IO Int
  ref <- newIORef (oneDiseased 0.1 3 2 3)
  iniSt <- readIORef ref
  putStrLn "Initial state:"
  putStrLn $ showModel 6 iniSt
  forM_ [1..rounds] . const $ do
    putStrLn "Press <Enter> for next step..."
    _ <- getLine
    modifyIORef ref step
    st <- readIORef ref
    putStrLn $ showModel 7 st
