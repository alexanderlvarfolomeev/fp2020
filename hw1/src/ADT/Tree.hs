module ADT.Tree
  ( Tree (..)
  , delete
  , find
  , fromList
  , insert
  , isEmpty
  , size
  ) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as List

-- | Binary Tree data type
data Tree a
  = Leaf
  | Branch (Tree a) (NonEmpty a) (Tree a)
  deriving Show

-- | equal trees is trees with the same structure
instance (Eq a) => Eq (Tree a) where
  Leaf                  == Leaf                  = True
  (Branch l1 items1 r1) == (Branch l2 items2 r2) =
    l1 == l2 && items1 == items2 && r1 == r2
  _                     == _                     = False

-- | checks if tree contains no elements
isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _    = False

-- | gets count of elements in the tree
size :: Tree a -> Int
size Leaf               = 0
size (Branch l items r) = size l + size r + length items

-- | checks if tree contains given element
find :: Ord a => a -> Tree a -> Bool
find _ Leaf             = False
find x (Branch l items r)
  | List.head items < x = find x r
  | List.head items > x = find x l
  | otherwise           = True

-- | add an element to the tree
insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf           = Branch Leaf (List.fromList [x]) Leaf
insert x (Branch l items r)
  | List.head items < x = Branch l items (insert x r)
  | List.head items > x = Branch (insert x l) items r
  | otherwise           = Branch l (x List.<| items) r

-- | constructs tree with elements from list
fromList :: Ord a => [a] -> Tree a
fromList = foldl (flip insert) Leaf

-- | deletes one element from the tree if it presents
delete :: Ord a => a -> Tree a -> Tree a
delete _ Leaf           = Leaf
delete x (Branch l items r)
  | List.head items < x = Branch l items (delete x r)
  | List.head items > x = Branch (delete x l) items r
  | otherwise           = if not . null $ List.tail items
                          then Branch l (List.fromList $ List.tail items) r
                          else hang l r

-- | merges two trees
hang :: Tree a -> Tree a -> Tree a
hang Leaf          r = r
hang l@Branch {} r = let (l', bound) = restruct l in Branch l' bound r

-- | returns rest of tree and the greatest elements
restruct :: Tree a -> (Tree a, NonEmpty a)
restruct (Branch l items Leaf)             = (l, items)
restruct (Branch l items r@Branch {}) = let (r', bound) = restruct r
                                             in (Branch l items r', bound)
restruct Leaf                              = undefined
