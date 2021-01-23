{-# LANGUAGE TupleSections #-}

module Task8
  ( CovidModel,
    oneDiseased,
    showModel,
    step
  ) where

import Control.Comonad
import Control.Monad (liftM2)
import System.Random (StdGen, random, mkStdGen)

data Condition
  = Healthy
  | Incubation Int
  | Symptoms Int
  | Immunity Int

instance Show Condition where
  show Healthy = "_"
  show (Incubation _) = "o"
  show (Symptoms _) = "#"
  show (Immunity _) = "@"

data ListZipper a = LZ [a] a [a] deriving Show
newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) } deriving Show

listLeft, listRight :: ListZipper a -> ListZipper a
listLeft  (LZ (a:as) x bs) = LZ as a (x:bs)
listLeft _ = error "listLeft"

listRight (LZ as x (b:bs)) = LZ (x:as) b bs
listRight _ = error "listRight"

listWrite :: a -> ListZipper a -> ListZipper a
listWrite x (LZ ls _ rs) = LZ ls x rs

toList :: Int -> ListZipper a -> [a]
toList n (LZ ls x rs) = reverse (take n ls) ++ [x] ++ take n rs

instance Functor ListZipper where
    fmap f (LZ ls x rs) = LZ (map f ls) (f x) (map f rs)

iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

genericMove :: (z a -> z a)
            -> (z a -> z a)
            -> z a
            -> ListZipper (z a)
genericMove f g e = LZ (iterateTail f e) e (iterateTail g e)

instance Comonad ListZipper where
  extract (LZ _ x _) = x
  duplicate = genericMove listLeft listRight

up, down :: Grid a -> Grid a
up   (Grid g) = Grid (listLeft  g)
down (Grid g) = Grid (listRight g)

left, right :: Grid a -> Grid a
left  (Grid g) = Grid (fmap listLeft  g)
right (Grid g) = Grid (fmap listRight g)

gridRead :: Grid a -> a
gridRead (Grid g) = extract $ extract g

gridWrite :: a -> Grid a -> Grid a
gridWrite x (Grid g) = Grid $ listWrite newLine g
  where
    oldLine = extract g
    newLine = listWrite x oldLine

horizontal, vertical :: Grid a -> ListZipper (Grid a)
horizontal = genericMove left right
vertical   = genericMove up   down

instance Functor Grid where
  fmap f = Grid . fmap (fmap f) . unGrid

instance Comonad Grid where
    extract = gridRead

    duplicate = Grid . fmap horizontal . vertical

listGrid :: Int -> Grid a -> [[a]]
listGrid n (Grid g) = toList n $ fmap (toList n) g

-- | Boxed grid that represents the curtain stage of disease
data CovidModel
  = CM (Grid (Condition, StdGen)) Double Int Int Int

infected :: Condition -> Bool
infected Healthy = False
infected (Immunity _) = False
infected _ = True

infectedCount :: [Condition] -> Int
infectedCount = length . filter infected

neighbours :: [Grid a -> Grid a]
neighbours = horizontals ++ verticals ++ liftM2 (.) horizontals verticals
  where horizontals = [left, right]
        verticals   = [up, down]

infectedNeighbours :: Grid (Condition, a) -> Int
infectedNeighbours g = infectedCount
                  $ map (\direction -> fst . extract $ direction g) neighbours

-- | Get @CovidModel@ with curtain infection probability
-- and stages durations and one diseased person
-- in the center
oneDiseased :: Double -> Int -> Int -> Int -> CovidModel
oneDiseased =
  let
    ini :: Int -> Int -> (Condition, StdGen)
    ini x y = (Healthy, mkStdGen (2 ^ x * 3 ^ y))
    iniLZ :: Int -> ListZipper (Condition, StdGen)
    iniLZ x = LZ (map (ini x) [2, 4..]) (ini x 0) (map (ini x) [1, 3..])

  in CM
      (gridWrite
        (Incubation 0, mkStdGen 0)
        (Grid
          (LZ
            (map iniLZ [2, 4..])
            (iniLZ 0)
            (map iniLZ [1, 3..]))))

-- | Get next stage of disease
step :: CovidModel -> CovidModel
step (CM grid p incub sym imun) = CM (extend rule grid) p incub sym imun where
  rule :: Grid (Condition, StdGen) -> (Condition, StdGen)
  rule grid' = case gridRead grid' of
    (Healthy, g) ->
      let (x, g') = random g :: (Double, StdGen)
      in if (1 - p) ** toEnum (infectedNeighbours grid') >= x
         then (Healthy, g')
         else (Incubation 0, g')
    (Incubation x, g) -> (, g) $
      if (x + 1) >= incub
      then Symptoms 0
      else Incubation (x + 1)
    (Symptoms x, g) -> (, g) $
      if (x + 1) >= sym
      then Immunity 0
      else Symptoms (x + 1)
    (Immunity x, g) -> (, g) $
      if (x + 1) >= imun
      then Healthy
      else Immunity (x + 1)

-- | Get representation of current disease situation
-- in curtain range from center
showModel :: Int -> CovidModel -> String
showModel n (CM grid _ _ _ _) =
  let l = fmap fst <$> listGrid n grid
      show' l' = unwords (map show l') ++ "\n"
  in concatMap show' l
