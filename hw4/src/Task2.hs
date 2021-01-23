module Task2
  ( integrate,
    integrateSeq
  ) where

import Control.Parallel.Strategies (rpar, rseq, runEval)
import GHC.Conc (numCapabilities)

import Data.List (foldl')

-- | Sequential implementation of monte carlo integration
integrateSeq :: (Double -> Double) -> Int -> Double -> Double -> Double
integrateSeq f n a b =
  let l = b - a
  in l / toEnum (n + 1) * foldl (\s i -> s + f (a + toEnum i * l / toEnum n)) (0.0 :: Double) [0..n]

-- | Parallel implementation of monte carlo integration
integrate :: (Double -> Double) -> Int -> Double -> Double -> Double
integrate f n a b =
  let l = b - a
  in (l / toEnum (n + 1) *) . runEval $ do
    let ranges = splitInclusiveRange numCapabilities 0 n
    let runPar = rpar . foldl' (+) 0.0 . map (\i -> f (a + toEnum i * l / toEnum n)) . uncurry enumFromTo
    parts <- traverse runPar ranges
    rseq $ foldr (+) 0.0 parts

splitInclusiveRange :: Int -> Int -> Int -> [(Int, Int)]
splitInclusiveRange pieces start' end' = helper pieces start' (end' + 1) where
  l = end' - start' + 1
  helper 0 _ _ = []
  helper n start end = (start + (n - 1) * l `div` pieces, start + n * l `div` pieces - 1) : helper (n - 1) start end
