module Task1
  ( Point (..),
    crossProduct,
    doubleArea,
    doubleAreaNaive,
    perimeter,
    perimeterNaive,
    plus,
    minus,
    scalarProduct
  ) where

import Control.DeepSeq (NFData (..), deepseq)
import Data.List (foldl')

newtype Point = Point { getPoint :: (Int, Int) }

instance NFData Point where
  rnf (Point p) = rnf p

-- | Points sum
plus          :: Point -> Point -> Point
plus (Point (x1, y1)) (Point (x2, y2)) = Point (x1 + x2, y1 + y2)

-- | Points minus
minus         :: Point -> Point -> Point
minus (Point (x1, y1)) (Point (x2, y2)) = Point (x1 - x2, y1 - y2)

-- | Scalar product of points as 2-dimentional vectors
scalarProduct :: Point -> Point -> Int
scalarProduct (Point (x1, y1)) (Point (x2, y2)) = x1 * x2 + y1 * y2

-- | Cross product of points
crossProduct  :: Point -> Point -> Int
crossProduct (Point (x1, y1)) (Point (x2, y2)) = x1 * y2 - x2 * y1

-- | Distance between two points
distance      :: Point -> Point -> Double
distance (Point (x1, y1)) (Point (x2, y2)) =
  let
  dx = x1 - x2
  dy = y1 - y2
  in sqrt . toEnum $ dx * dx + dy * dy

-- | Effective perimeter count
perimeter  :: [Point] -> Double
perimeter = action distance

-- | Effective double area count
doubleArea :: [Point] -> Int
doubleArea = abs . action crossProduct

-- | Naive perimeter count
perimeterNaive :: [Point] -> Double
perimeterNaive = actionNaive distance

-- | Naive double area count
doubleAreaNaive :: [Point] -> Int
doubleAreaNaive = abs . actionNaive crossProduct


action :: Num a => (Point -> Point -> a) -> [Point] -> a
action _ [] = 0
action f points@(pt : ps) = ps `deepseq` foldl' (\s (c, p) -> s + f c p) (f pt  (last ps)) (zip ps points)

actionNaive :: Num a => (Point -> Point -> a) -> [Point] -> a
actionNaive _ [] = 0
actionNaive f list = sum $ zipWith f list (tail $ cycle list)
