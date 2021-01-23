module Monoid.Semigroup
  ( Endo (..)
  , Name (..)
  , NonEmpty (..)
  , ThisOrThat (..)
  ) where

-- | data type that represents list which can't be empty
data NonEmpty a
  = a :| [a]
  deriving (Show, Eq)

instance Semigroup (NonEmpty a) where
  (head1 :| tail1) <> (head2 :| tail2) = head1 :| (tail1 ++ head2 : tail2)

-- | 'ThisOrThat' @a b@ contains @a@ value, @b@ value or both
data ThisOrThat a b
  = This a
  | That b
  | Both a b
  deriving (Show, Eq)

instance Semigroup (ThisOrThat a b) where
  this@(This _)   <> (This _)   = this
  (This a)        <> (That b)   = Both a b
  (This a)        <> (Both _ b) = Both a b
  that@(That _)   <> (That _)   = that
  (That b)        <> (This a)   = Both a b
  (That b)        <> (Both a _) = Both a b
  both@(Both _ _) <> _          = both

-- | Name contains string or empty
data Name
  = Name String
  | Empty
  deriving (Show, Eq)

instance Semigroup Name where
  (Name str1) <> (Name str2) = Name $ str1 ++ '.' : str2
  Empty       <> other       = other
  other       <> Empty       = other

instance Monoid Name where
  mempty = Empty

-- | data type that represents endomorphism
newtype Endo a
  = Endo { getEndo :: a -> a }

instance Semigroup (Endo a) where
  e1 <> e2 = Endo $ getEndo e1 . getEndo e2

instance Monoid (Endo a) where
  mempty = Endo id
