module Bonus
  ( Cont (..),
    ExitStatus (..),
    NoResult,
    Syscall,
    exit,
    fork,
    kernel,
    main,
    readLine,
    writeLine,
    yield
  ) where


-- | self-defined Cont monad data type
newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Functor (Cont r) where
  fmap f (Cont cont) = Cont $ \f' -> cont (f' . f)

instance Applicative (Cont r) where
  pure x = Cont ($ x)
  (Cont fcont) <*> (Cont xcont) = Cont $ \br -> fcont (\ab -> xcont (br . ab))

instance Monad (Cont r) where
  return = pure
  (Cont xcont) >>= f = Cont $ \g -> xcont (\x -> runCont (f x) g)


-- | exit status of process
data ExitStatus
  = Success
  | Failure
  deriving (Show, Eq)

-- | data type which represents supported system calls
data Syscall
  = ReadLine (String -> Syscall)
  | Write String (NoResult -> Syscall)
  | Yield (NoResult -> Syscall)
  | Exit ExitStatus (() -> Syscall)
  | Fork (Int -> Syscall)
  | Exec

-- | boxed unit type
newtype NoResult = NoResult ()

noRes :: NoResult
noRes = NoResult ()

-- | syscall for reading line from input
readLine :: Cont Syscall String
readLine = Cont ReadLine

-- | syscall for writing line to output
writeLine :: String -> Cont Syscall NoResult
writeLine = Cont . Write

-- | syscall for yielding current process execution
yield :: Cont Syscall NoResult
yield = Cont Yield

-- | create copy of current process
-- returns 0 to new process and PID of the new process to old one
-- if failures, returns -1
fork :: Cont Syscall Int
fork = Cont Fork

-- | stops process with certain @ExitStatus@
exit :: ExitStatus -> Cont Syscall ()
exit = Cont . Exit

-- | example of system calls usage
main :: IO ()
main = kernel main'

main' :: Cont Syscall ()
main' = do
  writeLine "Enter Your name:"
  x <- readLine
  pid <- fork
  if pid == 0
    then writeLine $ "Hello, " ++ show x ++ "!"
    else writeLine $ "Hello, " ++ show pid ++ "!"
  exit Success

-- | kernel simulation function
kernel :: Cont Syscall () -> IO ()
kernel ini = kernelDriver 0 [(0, runCont ini (const Exec))] where
  kernelDriver :: Int -> [(Int, Syscall)] -> IO ()
  kernelDriver _ [] = return ()
  kernelDriver mpid ((pid, cont) : rest) =
    case cont of
      ReadLine f -> do
        str <- getLine
        kernelDriver mpid $ rest ++ [(pid, f str)]
      Write str f -> do
        putStrLn str
        kernelDriver mpid $ rest ++ [(pid, f noRes)]
      Yield f -> do
        kernelDriver mpid $ rest ++ [(pid, f noRes)]
      Fork f -> do
        let npid = mpid + 1
        kernelDriver npid $ rest ++ [(pid, f npid), (npid, f 0)]
      Exit st _ -> do
        putStrLn $ "Process " ++ show pid ++ " stops with status: " ++ show st
        kernelDriver mpid rest
      Exec -> kernelDriver mpid rest

