module Main
    ( main
    ) where

import Control.DeepSeq (deepseq)
import Criterion.Main (bench, bgroup, defaultMain, whnf)
import Task1 (Point (..),
              doubleArea,
              doubleAreaNaive,
              perimeter,
              perimeterNaive)
import Task2 (integrate, integrateSeq)

-- | main for @stack bench@
main :: IO ()
main =
  let
    points = map (\i -> Point (i, 0)) [1..(10^(7 :: Int))]
    func = cos . (\x -> x ** 3 - 2 * x ** 2 + 17 * exp x)
  in points `deepseq` defaultMain [
      bgroup "perimeter" [ bench "naive" (whnf perimeterNaive points)
                         , bench "efficient" (whnf perimeter points)
                         ],
      bgroup "doubleArea" [ bench "naive" (whnf doubleAreaNaive points)
                          , bench "efficient" (whnf doubleArea points)
                          ],
      bgroup "integrate" [ bench "seq" $ whnf (integrateSeq func (10^(7 :: Int)) 0) 10
                         , bench "par" $ whnf (integrate func (10^(7 :: Int)) 0) 10
                         ]
                      ]
