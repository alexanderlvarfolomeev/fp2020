{-# LANGUAGE TupleSections #-}

module Monoid.Concat
  ( eitherConcat
  , maybeConcat
  ) where

import Data.Foldable (fold)

-- | appends monoids inside nested @Foldable@s
foldableConcat :: (Foldable f1, Foldable f2, Monoid m) => f1 (f2 m) -> m
foldableConcat = foldMap fold

-- | appends monoids inside nested @Foldable@ of @Maybe@
maybeConcat :: (Foldable f1, Monoid m) => f1 (Maybe m) -> m
maybeConcat = foldableConcat

-- | concats monoids inside @Foldable@ of @Either@
eitherConcat :: (Foldable f, Monoid m1, Monoid m2) => f (Either m1 m2) -> (m1, m2)
eitherConcat = foldMap $ either (, mempty) (mempty, )
