-- | Crib of Path.IO from path-io.

{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

module Path.IO
  ( listDir
  , getCurrentDir
  , getHomeDir
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO (..))
import Data.Either (lefts, rights)
import Data.List ((\\))
import Path
import qualified System.Directory as D
import qualified System.FilePath  as F

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid)
#endif

-- | @'listDir' dir@ returns a list of /all/ entries in @dir@ without the
-- special entries (@.@ and @..@). Entries are not sorted.
--
-- The operation may fail with:
--
-- * 'HardwareFault'
--   A physical I\/O error has occurred.
--   @[EIO]@
--
-- * 'InvalidArgument'
--   The operand is not a valid directory name.
--   @[ENAMETOOLONG, ELOOP]@
--
-- * 'isDoesNotExistError' \/ 'NoSuchThing'
--   The directory does not exist.
--   @[ENOENT, ENOTDIR]@
--
-- * 'isPermissionError' \/ 'PermissionDenied'
--   The process has insufficient privileges to perform the operation.
--   @[EACCES]@
--
-- * 'ResourceExhausted'
--   Insufficient resources are available to perform the operation.
--   @[EMFILE, ENFILE]@
--
-- * 'InappropriateType'
--   The path refers to an existing non-directory object.
--   @[ENOTDIR]@

listDir :: (MonadIO m, MonadThrow m)
  => Path b Dir        -- ^ Directory to list
  -> m ([Path Abs Dir], [Path Abs File]) -- ^ Sub-directories and files
listDir path = do
  bpath <- makeAbsolute path
  raw   <- liftD D.getDirectoryContents bpath
  items <- forM (raw \\ [".", ".."]) $ \item -> do
    let ipath = toFilePath bpath F.</> item
    isDir <- liftIO (D.doesDirectoryExist ipath)
    if isDir
      then Left  `liftM` parseAbsDir  ipath
      else Right `liftM` parseAbsFile ipath
  return (lefts items, rights items)

----------------------------------------------------------------------------
-- Current working directory

-- | Obtain the current working directory as an absolute path.
--
-- In a multithreaded program, the current working directory is a global
-- state shared among all threads of the process. Therefore, when performing
-- filesystem operations from multiple threads, it is highly recommended to
-- use absolute rather than relative paths (see: 'makeAbsolute').
--
-- The operation may fail with:
--
-- * 'HardwareFault'
-- A physical I\/O error has occurred.
-- @[EIO]@
--
-- * 'isDoesNotExistError' or 'NoSuchThing'
-- There is no path referring to the working directory.
-- @[EPERM, ENOENT, ESTALE...]@
--
-- * 'isPermissionError' or 'PermissionDenied'
-- The process has insufficient privileges to perform the operation.
-- @[EACCES]@
--
-- * 'ResourceExhausted'
-- Insufficient resources are available to perform the operation.
--
-- * 'UnsupportedOperation'
-- The operating system has no notion of current working directory.

getCurrentDir :: (MonadIO m, MonadThrow m) => m (Path Abs Dir)
getCurrentDir = liftIO D.getCurrentDirectory >>= parseAbsDir
{-# INLINE getCurrentDir #-}

----------------------------------------------------------------------------
-- Pre-defined directories

-- | Returns the current user's home directory.
--
-- The directory returned is expected to be writable by the current user,
-- but note that it isn't generally considered good practice to store
-- application-specific data here; use 'getAppUserDataDir' instead.
--
-- On Unix, 'getHomeDir' returns the value of the @HOME@ environment
-- variable. On Windows, the system is queried for a suitable path; a
-- typical path might be @C:\/Users\//\<user\>/@.
--
-- The operation may fail with:
--
-- * 'UnsupportedOperation'
-- The operating system has no notion of home directory.
--
-- * 'isDoesNotExistError'
-- The home directory for the current user does not exist, or
-- cannot be found.

getHomeDir :: (MonadIO m, MonadThrow m) => m (Path Abs Dir)
getHomeDir = liftIO D.getHomeDirectory >>= resolveDir'
{-# INLINE getHomeDir #-}

----------------------------------------------------------------------------
-- Path transformation

-- | Closed type family describing how to get absolute version of given
-- 'Path'.

type family AbsPath path where
  AbsPath (Path b File) = Path Abs File
  AbsPath (Path b Dir)  = Path Abs Dir

-- | Closed type family describing how to get relative version of given
-- 'Path'.
--
-- @since 0.3.0

type family RelPath path where
  RelPath (Path b File) = Path Rel File
  RelPath (Path b Dir)  = Path Rel Dir

-- | Class of things ('Path's) that can be canonicalized and made absolute.

class AnyPath path where

  -- | Make a path absolute and remove as many indirections from it as
  -- possible. Indirections include the two special directories @.@ and
  -- @..@, as well as any symbolic links. The input path need not point to
  -- an existing file or directory.
  --
  -- __Note__: if you require only an absolute path, use 'makeAbsolute'
  -- instead. Most programs need not care about whether a path contains
  -- symbolic links.
  --
  -- Due to the fact that symbolic links and @..@ are dependent on the state
  -- of the existing filesystem, the function can only make a conservative,
  -- best-effort attempt. Nevertheless, if the input path points to an
  -- existing file or directory, then the output path shall also point to
  -- the same file or directory.
  --
  -- Formally, symbolic links and @..@ are removed from the longest prefix
  -- of the path that still points to an existing file. The function is not
  -- atomic, therefore concurrent changes in the filesystem may lead to
  -- incorrect results.
  --
  -- (Despite the name, the function does not guarantee canonicity of the
  -- returned path due to the presence of hard links, mount points, etc.)
  --
  -- Similar to 'normalise', an empty path is equivalent to the current
  -- directory.
  --
  -- /Known bug(s)/: on Windows, the function does not resolve symbolic
  -- links.
  --
  -- Please note that before version 1.2.3.0 of @directory@ package, this
  -- function had unpredictable behavior on non-existent paths.

  canonicalizePath :: (MonadIO m, MonadThrow m)
    => path -> m (AbsPath path)

  -- | Make a path absolute by prepending the current directory (if it isn't
  -- already absolute) and applying 'normalise' to the result.
  --
  -- If the path is already absolute, the operation never fails. Otherwise,
  -- the operation may fail with the same exceptions as
  -- 'getCurrentDirectory'.

  makeAbsolute :: (MonadIO m, MonadThrow m)
    => path -> m (AbsPath path)

  -- | Make a path relative to given directory.
  --
  -- @since 0.3.0

  makeRelative :: MonadThrow m
    => Path Abs Dir    -- ^ Base directory
    -> path            -- ^ Path that will be made relative to base directory
    -> m (RelPath path)

  -- | Make a path relative to current working directory.
  --
  -- @since 0.3.0

  makeRelativeToCurrentDir :: (MonadIO m, MonadThrow m)
    => path -> m (RelPath path)

instance AnyPath (Path b File) where
  canonicalizePath = liftD D.canonicalizePath >=> parseAbsFile
  {-# INLINE canonicalizePath #-}
  makeAbsolute     = liftD D.makeAbsolute     >=> parseAbsFile
  {-# INLINE makeAbsolute #-}
  makeRelative b p = parseRelFile (F.makeRelative (toFilePath b) (toFilePath p))
  {-# INLINE makeRelative #-}
  makeRelativeToCurrentDir p = getCurrentDir >>= flip makeRelative p
  {-# INLINE makeRelativeToCurrentDir #-}

instance AnyPath (Path b Dir) where
  canonicalizePath = liftD D.canonicalizePath >=> parseAbsDir
  {-# INLINE canonicalizePath #-}
  makeAbsolute     = liftD D.makeAbsolute     >=> parseAbsDir
  {-# INLINE makeAbsolute #-}
  makeRelative b p = parseRelDir (F.makeRelative (toFilePath b) (toFilePath p))
  {-# INLINE makeRelative #-}
  makeRelativeToCurrentDir p = getCurrentDir >>= flip makeRelative p
  {-# INLINE makeRelativeToCurrentDir #-}

-- | The same as 'resolveFile', but for directories.
--
-- @since 0.3.0

resolveDir :: (MonadIO m, MonadThrow m)
  => Path Abs Dir      -- ^ Base directory
  -> FilePath          -- ^ Path to resolve
  -> m (Path Abs Dir)
resolveDir b p = f (toFilePath b F.</> p) >>= parseAbsDir
  where f = liftIO . D.canonicalizePath
{-# INLINE resolveDir #-}

-- | The same as 'resolveDir', but uses current working directory.
--
-- @since 0.3.0

resolveDir' :: (MonadIO m, MonadThrow m)
  => FilePath          -- ^ Path to resolve
  -> m (Path Abs Dir)
resolveDir' p = getCurrentDir >>= flip resolveDir p
{-# INLINE resolveDir' #-}

----------------------------------------------------------------------------
-- Helpers

-- | Lift action in 'IO' that takes 'FilePath' into action in slightly more
-- abstract monad that takes 'Path'.

liftD :: MonadIO m
  => (FilePath -> IO a) -- ^ Original action
  -> Path b t          -- ^ 'Path' argument
  -> m a               -- ^ Lifted action
liftD m = liftIO . m . toFilePath
{-# INLINE liftD #-}
