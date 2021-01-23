module ADT.Natural
  ( Nat (..)
  ) where

-- | data type that represents Peano natural numbers
data Nat
  = Z
  | S Nat
  deriving Show

-- | numbers the same if they have the same 'S' application count
instance Eq Nat where
  Z     == Z     = True
  (S n) == (S m) = n == m
  _     == _     = False

-- | @n <= m@, if @n@ has less or equal 'S' application count
instance Ord Nat where
  Z     <= _     = True
  (S n) <= (S m) = n <= m
  _     <= _     = False

-- | integer operations for Peano naterals.
-- if result of operation should be negative, throws error
instance Num Nat where
  n     + Z     = n
  n     + (S m) = S $ n + m
  _     * Z     = Z
  n     * (S m) = n * m + n
  n     - Z     = n
  Z     - (S _) = error "Negative result"
  (S n) - (S m) = n - m
  abs    n      = n
  signum _      = 1
  fromInteger n
    | n < 0 = error "Negative Argument"
    | n > 0 = S . fromInteger $ n - 1
    | otherwise = Z

-- | naturals as set with order
instance Enum Nat where
  toEnum n
    | n < 0     = error "Negative Argument"
    | otherwise = fromInteger $ toEnum n
  fromEnum      = fromEnum . toInteger

-- | Rational instance for natural numbers
instance Real Nat where
  toRational = toRational . toInteger

-- | naturals as integral numbers
instance Integral Nat where
  toInteger Z     = 0
  toInteger (S n) = 1 + toInteger n
  quotRem n d     = quotRemNat d n d Z
  divMod          = quotRem

-- | helper for 'quotRem' function
quotRemNat :: Nat -> Nat -> Nat -> Nat -> (Nat, Nat)
quotRemNat Z       _       _           _   = error "Division by zero"
quotRemNat divider n       Z           quo = quotRemNat divider n divider (S quo)
quotRemNat divider (S n)   (S r)       quo = quotRemNat divider n r quo
quotRemNat divider Z       rest@(S _)  quo = (quo, divider - rest)
