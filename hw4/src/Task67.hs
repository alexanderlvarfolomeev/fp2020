{-# LANGUAGE Rank2Types #-}

module Task67
  ( addSuffix,
    cd,
    dirName,
    fileName,
    file,
    fstDir,
    listFiles,
    ls,
    name,
    scan,
    setRoot,
    subContents
  ) where

import Data.Maybe (fromMaybe, isJust, isNothing)
import Lens.Micro (Lens',
                   Traversal',
                   filtered,
                   lens,
                   traversed,
                   (.~),
                   (<>~),
                   (^.),
                   (^..),
                   (^?))
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (dropTrailingPathSeparator, takeFileName, (</>))


data FS
    = Dir
    { _name     :: FilePath  -- название папки, не полный путь
    , _contents :: [FS]
    }
    | File
    { _name     :: FilePath  -- название файла, не полный путь
    }
    deriving Show

-- | Scans path to get file system tree representation
scan :: FilePath -> IO FS
scan fp = do
  isDir <- doesDirectoryExist fp
  if isDir
  then do
    list <- listDirectory fp
    fss <- mapM (scan . (fp </>)) list
    return $ Dir (takeFileName $ dropTrailingPathSeparator fp) fss
  else return . File . takeFileName $ fp

-- | Lens from @FS@ to its filepath
name :: Lens' FS FilePath
name = lens _name (\fs n' -> fs {_name = n'})

-- | Traversal from @Dir@ to its content
contents :: Traversal' FS [FS]
contents f (Dir n fs)  = Dir n <$> f fs
contents _ fl@(File _) = pure fl

-- | Get subtrees of @Dir@ or @[]@ if @FS@ is @File@
subContents :: FS -> [FS]
subContents fs = fromMaybe [] $ fs ^? contents

-- | Get directory name or Nothing
dirName :: FS -> Maybe FilePath
dirName fs = fs ^? contents >> return (fs ^. name)

-- | Get file name or Nothing
fileName :: FS -> Maybe FilePath
fileName fs = maybe (pure (fs ^. name)) (const Nothing) (fs ^? contents)

-- | Set name of current @FS@ to "/"
setRoot :: FS -> FS
setRoot = name .~ "/"

-- | Add suffix to the end of @FS@ name
addSuffix :: String -> FS -> FS
addSuffix = (name <>~)

-- | Get name of the first folder between @FS@ subtrees
fstDir :: FS -> Maybe FilePath
fstDir fs =
  fs ^? contents .
  traversed .
  filtered (isJust . (^? contents)) .
  name

-- | List all files between @FS@ subtrees
listFiles :: FS -> Maybe [FS]
listFiles fs =
  (^.. traversed . filtered (isNothing . (^? contents))) <$> (fs ^? contents)

-- | get Traversal to the subfolder with the curtain name
cd :: FilePath -> Traversal' FS FS
cd fp f dir@(Dir _ fss) =
  maybe (pure dir) f $ filter ((== Just fp) . dirName) fss ^? traversed
cd _ _ fl@(File _) = pure fl

-- | Get Traversal to the name of the curtain subfile
file :: FilePath -> Traversal' FS FilePath
file fp f fs =
  maybe (pure fs) (fmap File <$> f) $
    fs ^? contents . traversed . filtered ((== Just fp) . dirName) . name

-- | Get names of all subtrees
ls :: Traversal' FS FilePath
ls = contents . traversed . name
