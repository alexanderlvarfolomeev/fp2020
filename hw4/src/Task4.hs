{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Task4
  ( Halyava (..),
    HalyavaType (..),
    evaluate,
    evaluateFun1,
    sample,
    signSample
  ) where

import Control.Applicative (liftA2)
import Control.Monad (when)
import Control.Monad.ST
import Data.STRef

-- Primitive types supported in HalyavaScript
class (Ord a, Show a) => HalyavaType a where
  initial :: a

data Undefined = Undefined deriving (Eq, Ord)

instance Show Undefined where
  show Undefined = "undefined"

instance HalyavaType Undefined where
  initial = Undefined
instance HalyavaType Int where
  initial = 0
instance HalyavaType String where
  initial = ""
instance HalyavaType Bool where
  initial = False

-- | Final tagless method of HalyavaScript abstraction
class Halyava m where
  type Var m :: * -> *
  newVar :: a -> m (Var m a)
  hsVal :: HalyavaType a => a -> m a
  infix 4 @=
  (@=) :: HalyavaType a => Var m a -> m a -> m Undefined
  infixl 3 #
  (#) :: m a -> m b -> m b
  infixl 6 @+
  (@+) :: m Int -> m Int -> m Int
  infixl 6 @-
  (@-) :: m Int -> m Int -> m Int
  infixl 7 @*
  (@*) :: m Int -> m Int -> m Int
  infixl 7 @/
  (@/) :: m Int -> m Int -> m Int
  infixl 5 @>
  (@>) :: m Int -> m Int -> m Bool
  eRead :: Var m a -> m a
  sIf :: m Bool -> m a -> m a -> m a
  sWhile :: m Bool -> m a -> m ()
  sFun1
    :: (HalyavaType a, HalyavaType b)
    => (m a -> Var m b -> m c)
    -> (m a -> m b)
  sWithVar :: HalyavaType a => a -> (Var m a -> m b) -> m b

instance Halyava (ST s) where
  type Var (ST s) = STRef s
  hsVal = return
  newVar = newSTRef
  v @= x = do
    x' <- x
    writeSTRef v x'
    return Undefined
  (#) = (>>)
  (@+) = liftA2 (+)
  (@-) = liftA2 (-)
  (@*) = liftA2 (*)
  (@/) = liftA2 div
  (@>) = liftA2 (>)
  eRead v = readSTRef v
  sIf b t f = do
    b' <- b
    if b' then t else f
  sWhile b expr = do
    b' <- b
    when b' (expr >> sWhile b expr)
  sWithVar x f = do
    x' <- newSTRef x
    f x'
  sFun1 f x = do
    x' <- newSTRef initial
    _ <- f x x'
    readSTRef x'

-- | Evaluate primitive HS type
evaluate :: (forall s. ST s a) -> a
evaluate = runST

-- | Evaluate function HS type
evaluateFun1 :: (forall s. ST s a -> ST s b) -> a -> b
evaluateFun1 mf a = runST (mf $ return a)

-- | Integer pow function as HS code sample
sample :: Halyava m => m Int -> m Int
sample = sFun1 $ \a logCnt ->
  sWithVar 0 $ \accum ->
    accum @= hsVal 1 #
    logCnt @= hsVal 0 #
    sWhile (a @> eRead accum)
      ( accum @= eRead accum @+ eRead accum #
        logCnt @= eRead logCnt @+ hsVal 1
      )

-- | Sign function as HS code sample
signSample :: Halyava m => m Int -> m String
signSample = sFun1 $ \a res ->
  res @= sIf
    (a @> hsVal 0)
    (hsVal "Positive")
    (sIf
      (hsVal 0 @> a)
      (hsVal "Negative")
      (hsVal "Zero"))
