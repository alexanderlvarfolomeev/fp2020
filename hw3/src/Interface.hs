module Interface
  ( FSActions (..),
    FSException (..),
    PathInfo (..),
    fileType
  ) where

import Control.Monad (guard)
import Control.Monad.Catch (Exception, MonadCatch, throwM)
import Data.List (uncons)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime)
import System.Directory (Permissions)
import System.FilePath (dropTrailingPathSeparator, isAbsolute, isRelative,
                        isValid, splitPath, takeExtension, (</>))

-- | Exception in case of FSActions errors
newtype FSException = FSException String deriving (Eq)

instance Show FSException where
  show (FSException message) = "FSException: " ++ message

instance Exception FSException

-- | Data type, used by 'info' to show information about the file/directory
data PathInfo
  = FileInfo FilePath Permissions String UTCTime Integer
  | DirectoryInfo FilePath Int Permissions Integer
  deriving Eq


driveDoubleDotCheck :: FilePath -> FilePath -> Bool
driveDoubleDotCheck ab fp =
  let splitFP = fmap dropTrailingPathSeparator . splitPath
  in if isAbsolute fp
     then 0 < balance (fromMaybe [] $ tailMay (splitFP fp)) 1
     else 0 < balance (splitFP fp) 0 + length (splitFP ab) where
  balance :: [FilePath] -> Int -> Int
  balance [] b            = b
  balance ("." : rest) b  = balance rest b
  balance (".." : rest) b = balance rest (b - 1)
  balance (_ : rest) b    = min b $ balance rest (b + 1)


instance Show PathInfo where
  show (FileInfo fp perms mime tm size) =
    "File " ++ fp ++ ":\n" ++
    show perms ++ "\n" ++
    "Mime: " ++ mime ++ "\n" ++
    "Last Modification time: " ++ show tm ++ "\n" ++
    "Size: " ++ show size
  show (DirectoryInfo fp count perms size) =
    "Directory " ++ fp ++ ":\n" ++
    show perms ++ "\n" ++
    "Files inside: " ++ show count ++ "\n" ++
    "Size: " ++ show size

-- | Class, which represents abstraction of work with file system
class (MonadCatch m) => FSActions m where
  mkdir :: FilePath -> m ()
  touch :: FilePath -> m ()
  cd :: FilePath -> m ()
  ls :: FilePath -> m [FilePath]
  dir :: m [FilePath]
  cat :: FilePath -> m String
  pwd :: m FilePath
  del :: FilePath -> m ()
  echo :: FilePath -> String -> m ()
  find :: FilePath -> String -> m [FilePath]
  info :: FilePath -> m PathInfo
  isFile :: FilePath -> m Bool
  isDir :: FilePath -> m Bool
  joinToCurrent :: FilePath -> m FilePath
  canonicalize :: FilePath -> m FilePath

  joinToCurrent fp
    | not $ isValid fp = throwM $ FSException ("Invalid path: " ++ fp)
    | otherwise     = pwd >>= \cur ->
      if driveDoubleDotCheck cur fp
        then canonicalize . (</> fp) $ guard (isRelative fp) >> cur
        else throwM $ FSException
          ("Invalid path, <drive>/../<rest of path> doesn't exist: " ++
           fp ++ ". Current: " ++ cur
          )
  touch = flip echo ""
  dir = pwd >>= ls

tailMay :: [a] -> Maybe [a]
tailMay = fmap snd . uncons

fileType :: FilePath -> String
fileType = fromMaybe "other" . tailMay . takeExtension
