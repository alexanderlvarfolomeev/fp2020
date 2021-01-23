module Block1
  ( NonEmpty (..),
    Tree (..),
    stringSum
  ) where

import Control.Applicative (liftA2)
import Text.Read (readMaybe)

-- Task 1

-- | sums ints in the string or returns @Nothing@
-- if string contains invalid @Int@
stringSum :: String -> Maybe Int
stringSum = fmap sum . traverse readMaybe . words

-- Task 2

-- | data type that represents Tree data structure
data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a
  deriving (Eq, Show)

instance Functor Tree where
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Branch l r) = Branch (f <$> l) (f <$> r)

instance Applicative Tree where
  pure = Leaf
  (Leaf f) <*> xs = fmap f xs
  (Branch lf rf) <*> xs = Branch (lf <*> xs) (rf <*> xs)

instance Foldable Tree where
  foldMap f (Leaf x) = f x
  foldMap f (Branch l r) = foldMap f l `mappend` foldMap f r

instance Traversable Tree where
  sequenceA (Leaf x) = Leaf <$> x
  sequenceA (Branch l r) = liftA2 Branch (sequenceA l) (sequenceA r)

-- Task 3

-- | data type that represents non empty list
data NonEmpty a = a :| [a] deriving (Show, Eq)

instance Functor NonEmpty where
  fmap f (x :| xs) = f x :| fmap f xs

instance Applicative NonEmpty where
  pure x = x :| []
  (f :| fs) <*> l@(x :| xs) = let (x' :| xs') = f <$> l
                              in x' :| (xs' ++ (fs <*> (x : xs)))

instance Foldable NonEmpty where
  foldMap f (x :| xs) = f x `mappend` foldMap f xs

instance Traversable NonEmpty where
  sequenceA (x :| xs) = liftA2 (:|) x (sequenceA xs)

