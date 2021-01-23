module Lib
  ( repl
  ) where

import Control.Exception (SomeException)
import Control.Monad.Catch (MonadCatch, catch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Char (isSpace)
import Data.IORef (newIORef)
import Options.Applicative (Parser, ParserInfo, ParserResult (..), command,
                            defaultPrefs, execParserPure, fullDesc,
                            getParseResult, header, help, helper, hsubparser,
                            metavar, progDesc, renderFailure, strArgument,
                            value, (<**>), (<|>))
import qualified Options.Applicative as Opt
import Options.Applicative.Types (execCompletion)
import System.Directory (getCurrentDirectory)
import System.IO (hFlush, hPutStrLn, stderr, stdout)

import Interface (FSActions (..))
import qualified Interface as I
import Task (RealFS (..))


data Command
  = Mkdir FilePath
  | Touch FilePath
  | Cd    FilePath
  | Ls    FilePath
  | Dir
  | Cat   FilePath
  | Pwd
  | Del   FilePath
  | Echo  FilePath String
  | Find  FilePath String
  | Info FilePath
  | Help
  | End
  | Skip
  deriving (Eq, Show)

-- | Repl for working with the file system
repl :: IO ()
repl = getCurrentDirectory >>= newIORef >>= runReaderT (runRealFS repl') where
  repl' :: RealFS ()
  repl' = do
    pwd >>= RealFS  . liftIO . putStr . ( ++ "> ")
    RealFS . liftIO $ hFlush stdout
    str <- RealFS $ liftIO getLine
    let comm = execParserPure defaultPrefs tempInfo (hackedWords str)
    if getParseResult comm == Just End
      then return ()
      else handleParserResult comm >> repl'

hackedWords :: String -> [String]
hackedWords s = case dropWhile isSpace s of
                "" -> []
                s' -> w : if w /= "echo" then words s'' else split1 where
                    (w, s'') = break isSpace s'
                    split1 = case dropWhile isSpace s'' of
                             "" -> []
                             s3 -> let (w', s4) = break isSpace s3
                                   in w' : [dropWhile isSpace s4]


catchAll :: MonadCatch m => m a -> (SomeException -> m a) -> m a
catchAll = catch

catchExc :: (MonadIO m, MonadCatch m) => m () -> String -> m ()
catchExc op message =
  catchAll op
           (\e -> do
             let err = show e
             liftIO $ hPutStrLn stderr ("Exception: " ++ message ++ ":\n" ++ err)
             return ())

exec :: Either String Command -> RealFS ()
exec = either (liftIO . putStrLn . ("Exception: " ++)) execCom where
  execCom :: Command -> RealFS ()
  execCom (Cd fp)       = catchExc (cd fp) ("Couldn't change current directory to " ++ fp)
  execCom (Mkdir fp)    = catchExc (mkdir fp) ("Couldn't create new directory " ++ fp)
  execCom (Touch fp)    = catchExc (touch fp) ("Couldn't create new file " ++ fp)
  execCom (Ls fp)       = catchExc (ls fp >>= liftIO . putStrLn . unwords) ("Couldn't list directory " ++ fp)
  execCom Dir           = catchExc (dir >>= liftIO . putStrLn . unwords) "Couldn't list current directory"
  execCom (Cat fp)      = catchExc (cat fp >>= liftIO . putStrLn) ("Couldn't print the content of file " ++ fp)
  execCom Pwd           = catchExc (pwd >>= liftIO . putStrLn) "Couldn't get current directory"
  execCom (Del fp)      = catchExc (del fp) ("Couldn't delete file or directory on path " ++ fp)
  execCom (Echo fp msg) = catchExc (echo fp msg) ("Couldn't write message to " ++ fp)
  execCom (Find fp nm)  = catchExc (find fp nm >>= liftIO . putStrLn . unwords) ("Couldn't start the search in directory " ++ fp)
  execCom (Info fp)     = catchExc (I.info fp >>= liftIO . print) ("Couldn't get information about " ++ fp)
  execCom _             = return ()

pathParser :: String -> Parser String
pathParser msg = strArgument (metavar "filepath" <> help msg)

tempInfo :: ParserInfo Command
tempInfo = Opt.info (temp <**> helper)
  ( fullDesc <>
    header "Welcome to hash"
  )

temp :: Parser Command
temp = hsubparser
  ( command "exit"
      (Opt.info (pure End) (progDesc "End the session"))
    <> command "end"
      (Opt.info (pure End) (progDesc "Same as \"exit\""))
    <> command "help"
      (Opt.info (pure Help) (progDesc "Show this help text"))
    <> command "pwd"
      (Opt.info (pure Pwd) (progDesc "Print current directory"))
    <> command "dir"
      (Opt.info (pure Dir) (progDesc "List current directory"))
    <> command "ls"
      (Opt.info (fmap Ls (pathParser "Directory to list") <|> pure Dir) (progDesc "List given directory or current, if no arguments given"))
    <> command "touch"
      (Opt.info (Touch <$> pathParser "File to create") (progDesc "Create the file"))
    <> command "mkdir"
      (Opt.info (Mkdir <$> pathParser "Directory to create") (progDesc "Create the directory"))
    <> command "cat"
      (Opt.info (Cat <$> pathParser "File to read") (progDesc "Print file content in terminal"))
    <> command "del"
      (Opt.info (Del <$> pathParser "Path to delete") (progDesc "Delete file or directory"))
    <> command "info"
      (Opt.info (Info <$> pathParser "Path to get information") (progDesc "Get information about file/directory"))
    <> command "echo"
      (Opt.info (Echo <$> pathParser "File to write" <*> strArgument (metavar "content" <> help "New content")) (progDesc "Write to file from terminal"))
    <> command "find"
      (Opt.info (Find <$> pathParser "Directory" <*> strArgument (metavar "filename" <> help "File to find")) (progDesc "Find file in curtain directory recursively"))
    <> command "cd"
      (Opt.info (Cd <$> strArgument
        (metavar "filepath" <> help "Path to next directory" <> value ".."))
        (progDesc "Change current directory"))
  )

handleParserResult :: ParserResult Command -> RealFS ()
handleParserResult (Success Help) = handleParserResult $ execParserPure defaultPrefs tempInfo ["--help"]
handleParserResult (Success comm) = exec (Right comm)
handleParserResult (Failure failure) = liftIO . putStrLn . fst $ renderFailure failure ""
handleParserResult (CompletionInvoked compl) = liftIO (execCompletion compl "") >>= liftIO . putStrLn
