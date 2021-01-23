{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Task5
  ( showHS,
    showHSFun1
  ) where

import Control.Monad.Trans.State.Lazy
import Task4 (Halyava (..), HalyavaType (..))


newtype ToS a
  = ToS { toS :: String }
  deriving (Show, Semigroup, Monoid)

castToS :: ToS a -> ToS b
castToS (ToS s) = ToS s

newtype MToS a
  = MToS {getMToS :: State Int (ToS a) }

instance Halyava MToS where
  type Var MToS = ToS
  newVar _ = MToS $ do
    i <- get
    modify (+1)
    return . ToS $ 'v' : show i
  hsVal = MToS . return . ToS . show
  (MToS expr1) # (MToS expr2) = MToS $ do
    t1 <- expr1
    t2 <- expr2
    return $ castToS t1 <> ToS "\n" <> t2
  t @= (MToS x) = MToS $ do
    x' <- x
    return $ castToS t <> ToS " = " <> castToS x'
  (MToS x) @+ (MToS y) = MToS $ do
    x' <- x
    y' <- y
    return $ ToS "(" <> x' <> ToS ") + (" <> y' <> ToS ")"
  (MToS x) @- (MToS y) = MToS $ do
    x' <- x
    y' <- y
    return $ ToS "(" <> x' <> ToS ") - (" <> y' <> ToS ")"
  (MToS x) @* (MToS y) = MToS $ do
    x' <- x
    y' <- y
    return $ ToS "(" <> x' <> ToS ") * (" <> y' <> ToS ")"
  (MToS x) @/ (MToS y) = MToS $ do
    x' <- x
    y' <- y
    return $ ToS "(" <> x' <> ToS ") / (" <> y' <> ToS ")"
  (MToS x) @> (MToS y) = MToS $ do
    x' <- x
    y' <- y
    return . castToS $ ToS "(" <> x' <> ToS ") > (" <> y' <> ToS ")"
  eRead = MToS . return
  sIf (MToS b) (MToS t) (MToS f) = MToS $ do
    b' <- b
    t' <- t
    f' <- f
    return $
      ToS "if (" <>
      castToS b' <>
      ToS ") {\n" <>
      t' <>
      ToS "\n} else {\n" <>
      f' <>
      ToS "\n}"
  sWhile (MToS b) (MToS expr) = MToS $ do
    b' <- b
    expr' <- expr
    return $
      ToS "while (" <>
      castToS b' <>
      ToS ") {\n" <>
      castToS expr' <>
      ToS "\n}"
  sWithVar x f = MToS $ do
    v <- getMToS $ newVar x
    expr <- getMToS . f $ castToS v
    return $
      ToS "var " <>
      castToS v <>
      ToS (" = " ++ show x ++ "\n") <>
      expr
  sFun1 :: forall a b c. (HalyavaType a, HalyavaType b) =>
           (MToS a -> Var MToS b -> MToS c) -> MToS a -> MToS b
  sFun1 f (MToS x) = MToS $ do
    _ <- x
    arg <- getMToS $ newVar (initial :: a)
    res <- getMToS $ newVar (initial :: b)
    expr <- getMToS $ f (MToS . return $ castToS arg) (castToS res)
    return $
      ToS "function(" <>
      castToS arg <>
      ToS ") {\n" <>
      ToS "var " <>
      castToS res <>
      ToS (" = " ++ show (initial :: b) ++ "\n") <>
      castToS expr <>
      ToS "\nreturn " <>
      castToS res <>
      ToS "\n}"

-- | Show string representation of HS code with primitive type
showHS :: HalyavaType a => MToS a -> String
showHS (MToS st) = toS $ evalState st 0

-- | Show string representation of HS code with function type
showHSFun1 :: HalyavaType a => (MToS a -> MToS b) -> String
showHSFun1 f = toS . flip evalState 0 . getMToS $ f (hsVal initial)
