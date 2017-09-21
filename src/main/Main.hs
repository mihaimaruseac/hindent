{-# LANGUAGE Unsafe #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Main entry point to hindent.
--
-- hindent
module Main where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as S
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Version (showVersion)
import qualified Data.Yaml as Y
import           Descriptive
import           Descriptive.Options
import           Foreign.C.Error
import           GHC.IO.Exception
import           HIndent
import           HIndent.Types
import           Language.Haskell.Exts hiding (Style, style)
import           Path
import qualified Path.Find as Path
import qualified Path.IO as Path
import           Paths_hindent (version)
import qualified System.Directory as IO
import           System.Environment
import           System.Exit (exitWith)
import qualified System.IO as IO
import           Text.Read

-- | Main entry point.
main :: IO ()
main = do
  args <- getArgs
  config <- getConfig
  case consume (options config) (map T.pack args) of
    Succeeded (style, exts, action, mfilepath) ->
      case mfilepath of
        Just filepath -> do
          text <- S.readFile filepath
          case reformat style (Just exts) mfilepath text of
            Left e -> error e
            Right out ->
              unless (L8.fromStrict text == S.toLazyByteString out) $
                case action of
                  Validate -> do
                    IO.putStrLn $ filepath ++ " is not formatted"
                    exitWith (ExitFailure 1)
                  Reformat -> do
                    tmpDir <- IO.getTemporaryDirectory
                    (fp, h) <- IO.openTempFile tmpDir "hindent.hs"
                    L8.hPutStr h (S.toLazyByteString out)
                    IO.hFlush h
                    IO.hClose h
                    let exdev e =
                          if ioe_errno e == Just ((\(Errno a) -> a) eXDEV)
                            then IO.copyFile fp filepath >> IO.removeFile fp
                            else throw e
                    IO.copyPermissions filepath fp
                    IO.renameFile fp filepath `catch` exdev
        Nothing ->
          L8.interact
            (either error S.toLazyByteString . reformat style (Just exts) Nothing . L8.toStrict)
    Failed (Wrap (Stopped Version) _) ->
      putStrLn ("hindent " ++ showVersion version)
    Failed (Wrap (Stopped Help) _) -> putStrLn (help defaultConfig)
    _ -> error (help defaultConfig)

-- | Read config from a config file, or return 'defaultConfig'.
getConfig :: IO Config
getConfig = do
  cur <- Path.getCurrentDir
  homeDir <- Path.getHomeDir
  mfile <-
    Path.findFileUp cur ((== ".hindent.yaml") . toFilePath . filename) (Just homeDir)
  case mfile of
    Nothing -> return defaultConfig
    Just file -> do
      result <- Y.decodeFileEither (toFilePath file)
      case result of
        Left e -> error (show e)
        Right config -> return config

-- | Help text.
help :: Config -> String
help config =
  "hindent " ++
  T.unpack (textDescription (describe (options config) [])) ++
  "\nVersion " ++
  showVersion version ++
  "\n" ++
  "Default --indent-size is 2. Specify --indent-size 4 if you prefer that.\n" ++
  "-X to pass extensions e.g. -XMagicHash etc.\n" ++
  "The --style option is now ignored, but preserved for backwards-compatibility.\n" ++
  "Johan Tibell is the default and only style."

-- | Options that stop the argument parser.
data Stoppers
  = Version
  | Help
   deriving (Show)

data Action = Validate | Reformat

-- | Program options.
options
  :: Monad m
  => Config -> Consumer [Text] (Option Stoppers) m (Config, [Extension], Action, Maybe FilePath)
options config = ver *> ((,,,) <$> style <*> exts <*> action <*> file)
  where
    ver =
      stop (flag "version" "Print the version" Version) *>
      stop (flag "help" "Show help" Help)
    style =
      makeStyle <$>
      fmap
        (const config)
        (optional
           (constant "--style" "Style to print with" () *> anyString "STYLE")) <*>
      lineLen <*>
      indentSpaces <*>
      trailingNewline <*>
      sortImports
    exts = fmap getExtensions (many (prefix "X" "Language extension"))
    indentSpaces =
      fmap
        (>>= (readMaybe . T.unpack))
        (optional
           (arg "indent-size" "Indentation size in spaces, default: 4" <|>
            arg "tab-size" "Same as --indent-size, for compatibility"))
    lineLen =
      fmap
        (>>= (readMaybe . T.unpack))
        (optional (arg "line-length" "Desired length of lines"))
    trailingNewline =
      optional
        (constant "--no-force-newline" "Don't force a trailing newline" False)
    sortImports =
      optional
        (constant "--sort-imports" "Sort imports in groups" True <|>
         constant "--no-sort-imports" "Don't sort imports" False)
    action = fromMaybe Reformat <$>
      optional (constant "--validate" "Check if files are formatted without changing them" Validate)
    makeStyle s mlen tabs trailing imports =
      s
      { configMaxColumns = fromMaybe (configMaxColumns s) mlen
      , configIndentSpaces = fromMaybe (configIndentSpaces s) tabs
      , configTrailingNewline = fromMaybe (configTrailingNewline s) trailing
      , configSortImports = fromMaybe (configSortImports s) imports
      }
    file = fmap (fmap T.unpack) (optional (anyString "[<filename>]"))
