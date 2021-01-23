{-# LANGUAGE InstanceSigs #-}

module Fold.Tree where

import ADT.Tree (Tree (..))

-- | Foldable instance for 'ADT.Tree.Tree'
instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f ini Leaf               = ini
  foldr f ini (Branch l items r) = foldr f (foldr f (foldr f ini r) items) l
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f Leaf               = mempty
  foldMap f (Branch l items r) = foldMap f l `mappend` foldMap f items `mappend` foldMap f r
