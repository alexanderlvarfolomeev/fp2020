module Fold.Split
  ( joinWith
  , splitOn
  ) where

import Data.List.NonEmpty (NonEmpty (..))

-- | 'splitOn' @sep list@ splits the @list@ on sublists
-- with @sep@ as delimiter
splitOn :: Ord a => a -> [a] -> NonEmpty [a]
splitOn sep list = uncurry (:|) $ foldr append ([],[]) list where
  append element (sub, acc) = if element == sep
                              then ([], sub : acc)
                              else (element : sub, acc)

-- | 'joinWith' @sep lists@ joins @lists@ in one with @sep@ as delimiter
joinWith :: a -> NonEmpty [a] -> [a]
joinWith sep (h :| t) = h ++ foldr (\sublist acc -> sep : sublist ++ acc) [] t
