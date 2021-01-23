module Block2
  ( ArithmeticError (..),
    Expr (..),
    eval,
    moving
  ) where

import Control.Applicative (liftA2)
import Control.Monad.Trans.State (State, evalState, get, modify, state)

-- Task 1

-- | data type which represents arithmetic expression
data Expr = Con Int | Add Expr Expr | Subtract Expr Expr | Multiply Expr Expr | Divide Expr Expr | Pow Expr Expr deriving (Show, Eq)

-- | arithmetic exceptions
data ArithmeticError = DivisionByZero | NegativeExponent deriving (Show, Eq)


-- | evaluate given @Expr@
eval :: Expr -> Either ArithmeticError Int
eval (Con x)        = Right x
eval (Add x y)      = liftA2 (+) (eval x) (eval y)
eval (Subtract x y) = liftA2 (-) (eval x) (eval y)
eval (Multiply x y) = liftA2 (*) (eval x) (eval y)
eval (Divide x y)   = do
  a <- eval x
  b <- eval y
  if b == 0
  then Left DivisionByZero
  else return $ a `div` b
eval (Pow x y)      = do
  a <- eval x
  b <- eval y
  if b < 0
  then Left NegativeExponent
  else return $ a ^ b

-- Task 2

data Queue a = Queue [a] [a]

newQueue :: Queue a
newQueue = Queue [] []

enqueue :: Num a => a -> (a, Queue a) -> (a, Queue a)
enqueue x (s, Queue h t) =  (s + x, Queue h (x : t))

reverseQueue :: [a] -> [a] -> Queue a
reverseQueue h []      = Queue h []
reverseQueue h (x : t) = reverseQueue (x : h) t

dequeue :: Num a => (a, Queue a) -> (a, Queue a)
dequeue (s, q@(Queue [] []))    = (s, q)
dequeue (s, Queue [] t@(_ : _)) = dequeue (s, reverseQueue [] t)
dequeue (s, Queue (x : h) t)    = (s - x, Queue h t)

-- | Simple Moving Average algorithm realisation
moving :: (Show a, Fractional a) => Int -> [a] -> [a]
moving n list = evalState (movingHelper2 n 1 list) (0, newQueue)

movingHelper2 :: (Show a, Fractional a) => Int -> Int -> [a] -> State (a, Queue a) [a]
movingHelper2 _ _ [] = return []
movingHelper2 n m (x : xs) = do
  modify $ enqueue x
  s' <- if m <= n
        then fst <$> get
        else state $ \q -> let p = dequeue q in (fst p, p)
  xs' <- movingHelper2 n (m + 1) xs
  return ((s' / realToFrac (min m n)) : xs')
