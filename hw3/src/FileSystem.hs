{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FileSystem
  ( System (..),
    content,
    createDir,
    delPath,
    dirInf,
    dirs,
    dropPS,
    emptySys,
    getDir,
    getFile,
    fileInf,
    files,
    findFP,
    isDir_,
    path,
    throwFS,
    write
  ) where

import Control.Monad (join, when)
import Control.Monad.Catch ()
import Data.Bifunctor (first)
import Data.Either (isRight)
import Data.Function (on)
import Data.List (find, uncons)
import Data.Time.Calendar (Day (..))
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import System.Directory (emptyPermissions)
import System.FilePath (dropTrailingPathSeparator, splitFileName, splitPath,
                        (</>))

import Interface (FSException (..), PathInfo (..), fileType)

-- | Box exception message in 'FSException'
throwFS :: String -> Either FSException a
throwFS = Left . FSException

newtype Time = Time Integer deriving (Enum, Show)

timeToUTCTime :: Time -> UTCTime
timeToUTCTime (Time x) =
  UTCTime (ModifiedJulianDay 59204) (secondsToDiffTime x)

-- | File representation
data File =
  File {fileInf :: PathInfo, content :: String} deriving Show

-- | Directory representation
data Directory =
  Directory {dirInf :: PathInfo, files :: [File], dirs :: [Directory]}

instance Show Directory where
  show d = path (dirInf d) ++ ": " ++ show (dirs d)

-- | Toy file system for testing
data System =
  System {time :: Time, current :: FilePath, root :: Directory} deriving Show

-- | New 'System' with root directory
emptySys :: System
emptySys = let r = Directory (DirectoryInfo "/" 0 emptyPermissions 0) [] []
           in System (Time 1) "/" r

-- | Get name of file/directory
path :: PathInfo -> FilePath
path (FileInfo fp _ _ _ _)    = fp
path (DirectoryInfo fp _ _ _) = fp

sub :: FilePath -> Directory -> Either FSException Directory
sub name d =
  maybe (throwFS $ "there is no directory " ++ name) Right $
    find ((name ==) . path . dirInf) (dirs d)

subFile :: FilePath -> Directory -> Either FSException File
subFile name d =
  maybe (throwFS $ "there is no file " ++ name) Right $
    find ((name ==) . path . fileInf) (files d)

-- | Short name of 'System.FilePath.dropTrailingPathSeparator'
dropPS :: FilePath -> FilePath
dropPS = dropTrailingPathSeparator

-- | Get directory with given path
getDir :: FilePath -> Directory -> Either FSException Directory
getDir fp d = getDirs fp d >>= maybe (throwFS "") (return . fst) . uncons

getDirs :: FilePath -> Directory -> Either FSException [Directory]
getDirs fp d = getDirSplitted (dropPS <$> splitPath fp) [] d where
  getDirSplitted [] ds d' =
    Right (d' : ds)
  getDirSplitted ("/" : rest) ds d' =
    getDirSplitted rest ds d'
  getDirSplitted (smth : rest) ds d' =
    sub smth d >>= getDirSplitted rest (d' : ds)

-- | Get file with given path
getFile :: FilePath -> Directory -> Either FSException File
getFile fp d = do
  let (dirPath, name) = first dropPS $ splitFileName fp
  direct <- getDir dirPath d
  subFile name direct

-- | Check if given filepath points to directory
isDir_ :: FilePath -> System -> Bool
isDir_ c s = isRight $ getDir c (root s)

-- | Creates directory with given path
createDir :: FilePath -> System -> Either FSException System
createDir fp =
  sysMod
    (\_ _ -> throwFS $ "Directory " ++ fp ++ " already exists")
    (\_ _ -> throwFS $ fp ++ " is directory")
    (\name d -> Right (d {dirs = newDirectory name : dirs d}, 0, 0))
    fp

elif :: Bool -> Bool -> a -> a -> a -> a
elif b1 b2 x1 x2 x3
  | b1        = x1
  | b2        = x2
  | otherwise = x3

sysMod ::
  (FilePath -> Directory -> Either FSException (Directory, Int, Integer)) ->
  (FilePath -> Directory -> Either FSException (Directory, Int, Integer)) ->
  (FilePath -> Directory -> Either FSException (Directory, Int, Integer)) ->
  FilePath -> System -> Either FSException System
sysMod f1 f2 f3 fp s = do
  let (dirPath, name) = first dropPS $ splitFileName fp
  when (null name) (throwFS "Name can't be empty")
  ds <- getDirs dirPath (root s)
  (d, ds') <- maybe (throwFS "There is no parent directory") Right (uncons ds)
  let f = elif (isRight (sub name d)) (isRight (subFile name d)) f1 f2 f3
  either
    Left
    (\e' -> return s{root = let (d', c, s') = e' in dirsChangeTo c s' ds' d'})
    (f name d) where
    dirsChangeTo c' s' [] d =
      d {dirInf = incDirSize (dirInf d) c' s'}
    dirsChangeTo c' s' (d' : rest) d =
      dirsChangeTo c' s' rest $
        dirChangeTo (d {dirInf = incDirSize (dirInf d) c' s'}) d'

dirChangeTo :: Directory -> Directory -> Directory
dirChangeTo subD d =
  d { dirs =
      map (\x -> if on (==) (path . dirInf) x subD then subD else x)
      (dirs d)
    }

fileChangeTo :: File -> Directory -> Directory
fileChangeTo f d =
  d { files =
      map (\x -> if on (==) (path . fileInf) x f then f else x)
      (files d)
    }

newDirectory :: FilePath -> Directory
newDirectory name = Directory (DirectoryInfo name 0 emptyPermissions 0) [] []

newFile :: FilePath -> UTCTime  -> String -> File
newFile name t str =
  File
    (FileInfo name emptyPermissions (fileType name) t (toInteger $ length str))
    str

-- | Delete file/directory with fiven path
delPath :: FilePath -> System -> Either FSException System
delPath fp =
  sysMod
    (\name d ->
      maybe
        (throwFS "")
        (Right . getPathSize . dirInf)
        (find (\x -> path (dirInf x) == name) (dirs d)) >>= \(c', s') ->
          Right (d { dirs = filter
                            (\x -> path (dirInf x) /= name)
                            (dirs d)
                   }, -c', -s'))
    (\name d ->
      maybe
        (throwFS "")
        (Right . getPathSize . fileInf)
        (find (\x -> path (fileInf x) == name) (files d)) >>= \(c', s') ->
          Right (d { files =
                     filter
                     (\x -> path (fileInf x) /= name)
                     (files d)
                   }, -c', -s'))
    (\_ _ -> throwFS $ "Path" ++ fp ++ " doesn't exist")
    fp

-- | Write @str@ to file with fiven path
write :: FilePath -> String -> System -> Either FSException System
write fp str s = do
  let (dirPath, name) = first dropPS $ splitFileName fp
  when (null name) (throwFS "Name can't be empty")
  ds <- getDirs dirPath (root s)
  (d, ds') <- maybe (throwFS "There is no parent directory") Right (uncons ds)
  elif (isRight (sub name d)) (isRight (subFile name d))
    (throwFS $ fp ++ " is directory")
    (return s { root =
                dirsChangeTo
                  ds'
                  (fileChangeTo (newFile name (timeToUTCTime $ time s) str) d)
              })
    (return s { root =
                dirsChangeTo
                  ds'
                  (d { files =
                       newFile
                        name
                        (timeToUTCTime $ time s)
                        str
                      : files d
                     })
              }) where
      dirsChangeTo [] d =
        d {dirInf = incDirSize (dirInf d) 1 (toInteger $ length str)}
      dirsChangeTo (d' : rest) d =
        dirsChangeTo rest $ dirChangeTo (d { dirInf =
                                             incDirSize
                                              (dirInf d)
                                              1
                                              (toInteger $ length str)
                                           }) d'

incDirSize :: PathInfo -> Int -> Integer -> PathInfo
incDirSize (DirectoryInfo p c perms s) c' s' =
  DirectoryInfo p (c + c') perms (s + s')
incDirSize fi@FileInfo {}              _  _  =
  fi

getPathSize :: PathInfo -> (Int, Integer)
getPathSize (DirectoryInfo _ c _ s) = (c, s)
getPathSize (FileInfo _ _ _ _ s)    = (1, s)

-- | Find @fp@ recursively in given directory
findFP :: FilePath -> FilePath -> Directory -> Either FSException [FilePath]
findFP dirFP fp d = do
  let f = fmap ((dirFP </>) . path . fileInf) (subFile fp d)
  subs <- traverse (\x -> findFP (dirFP </> path (dirInf x)) fp x) (dirs d)
  return $ either (const $ join subs) (: join subs) f
