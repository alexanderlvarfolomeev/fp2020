{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Task
  ( MockFS (..),
    RealFS (..)
  ) where

import Control.Monad (join)
import Control.Monad.Catch (Exception, MonadCatch, MonadThrow, catch, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (ExceptT (..), catchE, throwE)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Control.Monad.Trans.State.Strict (State, state)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Tuple.Extra ((&&&))
import Data.Typeable ((:~:) (..), eqT)
import qualified System.Directory as Dir
import System.Directory.Extra (listContents, listDirectories, listFilesInside)
import System.FilePath (joinPath, splitPath, takeFileName)

import FileSystem (System (..), content, createDir, delPath, dirInf, dirs,
                   dropPS, fileInf, files, findFP, getDir, getFile, isDir_,
                   path, throwFS, write)
import Interface (FSActions (..), FSException (..), PathInfo (..), fileType)

-- | Monad for FSActions instance for real file system
newtype RealFS a
  = RealFS { runRealFS :: ReaderT (IORef FilePath) IO a }
  deriving (Functor, Applicative, Monad, MonadThrow, MonadCatch, MonadIO)

-- | Monad for FSActions instance for test file system
newtype MockFS a
  = MockFS { runMockFS :: ExceptT FSException (State System) a }
  deriving (Functor, Applicative, Monad)

instance FSActions RealFS where
  canonicalize = RealFS . liftIO . Dir.canonicalizePath
  mkdir fp = do
    work <- joinToCurrent fp
    RealFS . liftIO $ Dir.createDirectoryIfMissing True work
  del fp = do
    work <- joinToCurrent fp
    b <- isDir work
    if b
      then RealFS . liftIO $ Dir.removeDirectoryRecursive work
      else RealFS . liftIO $ Dir.removeFile work
  ls fp = joinToCurrent fp >>= RealFS . liftIO . fmap (fmap takeFileName) . listContents
  pwd = RealFS $ ask >>= liftIO . readIORef
  cd fp = do
    work <- joinToCurrent fp
    b <- isDir work
    if b
      then RealFS $ ask >>= liftIO . flip writeIORef work
      else throwM $ FSException ("Directory " ++ fp ++ " doesn't exists.")
  cat fp = joinToCurrent fp >>= RealFS . liftIO . readFile
  echo fp str = joinToCurrent fp >>= RealFS . liftIO . flip writeFile str
  find fp str = joinToCurrent fp >>= RealFS . liftIO . find' . (: []) where
    find' :: [FilePath] -> IO [FilePath]
    find' [] = return []
    find' fps@(_ : _) = do
      there <- Dir.findFiles fps str
      fps' <- traverse listDirectories fps
      (there ++) <$> find' (join fps')

  info fp = do
    work <- joinToCurrent fp
    b <- isDir work
    RealFS . liftIO $ if b then infoD work else infoF work
        where
          infoF = \work -> do
            size <- Dir.getFileSize work
            permissions <- Dir.getPermissions work
            modification <- Dir.getModificationTime work
            let mime = fileType work
            return $ FileInfo work permissions mime modification size
          infoD = \work -> do
            _files <- listFilesInside (fmap not . Dir.pathIsSymbolicLink) work
            size <- sum <$> mapM Dir.getFileSize _files
            let fileCount = length _files
            permissions <- Dir.getPermissions work
            return $ DirectoryInfo work fileCount permissions size
  isFile fp = joinToCurrent fp >>= RealFS . liftIO . Dir.doesFileExist
  isDir fp = joinToCurrent fp >>= RealFS . liftIO . Dir.doesDirectoryExist

instance {-# OVERLAPS #-} MonadThrow MockFS where
  throwM = MockFS . throwE . FSException . show

instance {-# OVERLAPS #-} MonadCatch MockFS where
  catch :: forall e a. Exception e => MockFS a -> (e -> MockFS a) -> MockFS a
  catch m handler = maybe m (\h -> MockFS $ catchE (runMockFS m) (runMockFS . h)) $ do
      Refl <- eqT @e @FSException
      return handler

state2monad :: (System -> Either FSException System) -> MockFS ()
state2monad ethr = MockFS . ExceptT . state $ \s ->
  either (Left &&& const (incTime s)) (const (Right ()) &&& incTime) (ethr s)

result2monad :: (System -> Either FSException a) -> MockFS a
result2monad ethr = MockFS . ExceptT . state $ (ethr &&& incTime)

incTime :: System -> System
incTime (System t c r) = System (succ t) c r

ifFun :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
ifFun p t f a = if p a then t a else f a

instance FSActions MockFS where
  canonicalize =
    result2monad . const .
    fmap (joinPath . reverse) .
    canonicalize1 [] . fmap dropPS . splitPath where
      canonicalize1
        :: [FilePath]
        -> [FilePath]
        -> Either FSException [FilePath]
      canonicalize1 can [] = Right can
      canonicalize1 can ("." : rest) = canonicalize1 can rest
      canonicalize1 [] (".." : rest) = canonicalize1 [] rest
      canonicalize1 [_] (".." : _) = throwFS ".. doesn't exists"
      canonicalize1 (_ : can@(_ : _)) (".." : rest) = canonicalize1 can rest
      canonicalize1 can (smth : rest) = canonicalize1 (smth : can) rest
  mkdir fp = joinToCurrent fp >>= state2monad . createDir
  cd fp = joinToCurrent fp >>= \work ->
    state2monad (\s -> if isDir_ work s
                       then return s {current = work}
                       else throwFS $ work ++ " is not a directory")
  ls fp = joinToCurrent fp >>= \work ->
    result2monad (\s ->
                  getDir work (root s) >>= \d ->
                  Right . fmap path $
                  (fileInf <$> files d) ++ (dirInf <$> dirs d))
  cat fp = joinToCurrent fp >>= \work ->
    result2monad (fmap content . getFile work . root)
  pwd = result2monad (Right . current)
  del fp = joinToCurrent fp >>= state2monad . delPath
  echo fp str = joinToCurrent fp >>= state2monad . flip write str
  find fp str = joinToCurrent fp >>= \work ->
    result2monad (\s -> getDir work (root s) >>= findFP "" str)
  info fp = joinToCurrent fp >>= \work -> result2monad (ifFun (isDir_ work) (fmap dirInf . getDir work . root) (fmap fileInf . getFile work . root))
  isFile fp = do
    b <- isDir fp
    return (not b)
  isDir fp = joinToCurrent fp >>= \work -> result2monad (Right . isDir_ work)
