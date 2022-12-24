{-# LANGUAGE DataKinds #-}

-- | Finding files.
-- Lifted from Stack.
module Path.Find
  ( findFileUp
  ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.List
import           Path
import           Path.IO                hiding (findFiles)

-- | Find the location of a file matching the given predicate.
findFileUp ::
     (MonadIO m, MonadThrow m)
  => Path Abs Dir -- ^ Start here.
  -> (Path Abs File -> Bool) -- ^ Predicate to match the file.
  -> Maybe (Path Abs Dir) -- ^ Do not ascend above this directory.
  -> m (Maybe (Path Abs File)) -- ^ Absolute file path.
findFileUp = findPathUp snd

-- | Find the location of a path matching the given predicate.
findPathUp ::
     (MonadIO m, MonadThrow m)
  => (([Path Abs Dir], [Path Abs File]) -> [Path Abs t])
              -- ^ Choose path type from pair.
  -> Path Abs Dir -- ^ Start here.
  -> (Path Abs t -> Bool) -- ^ Predicate to match the path.
  -> Maybe (Path Abs Dir) -- ^ Do not ascend above this directory.
  -> m (Maybe (Path Abs t)) -- ^ Absolute path.
findPathUp pathType dir p upperBound = do
  entries <- listDir dir
  case find p (pathType entries) of
    Just path -> return (Just path)
    Nothing
      | Just dir == upperBound -> return Nothing
      | parent dir == dir -> return Nothing
      | otherwise -> findPathUp pathType (parent dir) p upperBound
