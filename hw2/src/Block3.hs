module Block3
  ( Parser (..),
    cbs,
    element,
    eof,
    integerParser,
    listlistParser,
    ok,
    satisfy,
    stream
  ) where

import Data.Bifunctor (first)
import Control.Applicative (Alternative, empty, liftA2, many, some, (<|>))
import Data.Char (isDigit, isSpace)
import Control.Monad (void)


-- | Parser data type
newtype Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

-- Task 1

instance Functor (Parser s) where
  fmap f parser = Parser $ fmap (first f) . runParser parser

instance Applicative (Parser s) where
  pure x = Parser $ \s -> Just (x, s)
  p1 <*> p2 = Parser $ \s -> do
    (f, s') <- runParser p1 s
    (x, s'') <- runParser p2 s'
    return (f x, s'')

instance Alternative (Parser s) where
  empty = Parser (const Nothing)
  p1 <|> p2 = Parser $ \s -> runParser p1 s <|> runParser p2 s

instance Monad (Parser s) where
  return = pure
  p >>= f = Parser $ \s -> do
    (a, s') <- runParser p s
    runParser (f a) s'

-- Task 2

-- | Parser which always succeed,
-- returning given argument and not consuming input
ok :: a -> Parser s a
ok = pure

-- | Parser which succeed only if end of input reached
eof ::  Parser s ()
eof = Parser f where
  f [] = pure ((), [])
  f (_ : _) = empty

-- | Parser which succeed if predicate p returns @True@
-- on first element of input
satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser f where
  f [] = Nothing
  f (x : rest) = if p x
                 then pure (x, rest)
                 else empty

-- | Parser which succeed
-- if first element of input is equal to a given argument
element :: Eq s => s -> Parser s s
element s = satisfy (s ==)

-- | Parser which succeed
-- if prefix of input are equal to a given sequence
stream :: Eq s => [s] -> Parser s [s]
stream = foldr (liftA2 (:) . element) (ok [])

-- Task 3

-- | Parser which succeed
-- if input is correct bracket sequence
cbs :: Parser Char ()
cbs = cbsHelper *> eof where
  cbsHelper :: Parser Char ()
  cbsHelper = element '(' *> cbsHelper *> element ')' *> cbsHelper <|> ok ()

-- | Parser which succeed
-- if input starts with correct integer
integerParser :: Num a => Parser Char a
integerParser = fromInteger . read <$>
  (liftA2 (:) (element '-') unsignedIntegerParser <|>
   (element '+' <|> ok '+') *> unsignedIntegerParser) where
     unsignedIntegerParser :: Parser Char String
     unsignedIntegerParser = some $ satisfy isDigit

-- Task 4

-- | Parser which succeed
-- if input starts with correct @[[Int]]@ description
listlistParser :: Parser Char [[Int]]
listlistParser = liftA2 (:)
                        listParser
                        (many $ delims_ *> listParser) <|> ok [] where
  spaces_ :: Parser Char ()
  spaces_ = void $ many $ satisfy isSpace
  delims_ :: Parser Char ()
  delims_ = spaces_ *> element ',' *> spaces_
  ntimes :: Int -> Parser Char a -> Parser Char [a]
  ntimes n p = iterate (liftA2 (:) p) (ok []) !! n
  listParser :: Parser Char [Int]
  listParser = integerParser >>= flip ntimes (delims_ *> integerParser)
