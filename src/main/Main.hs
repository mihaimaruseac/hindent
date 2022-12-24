{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe            #-}

-- | Main entry point to hindent.
--
-- hindent
module Main
  ( main
  ) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString                 as S
import qualified Data.ByteString.Builder         as S
import qualified Data.ByteString.Lazy.Char8      as L8
import           Data.Maybe
import qualified Data.Text                       as T
import           Data.Version                    (showVersion)
import qualified Data.Yaml                       as Y
import           Foreign.C.Error
import           GHC.IO.Exception
import           HIndent
import           HIndent.CabalFile
import           HIndent.LanguageExtension
import           HIndent.LanguageExtension.Types
import           HIndent.Types
import           Options.Applicative             hiding (action, style)
import           Path
import qualified Path.Find                       as Path
import qualified Path.IO                         as Path
import           Paths_hindent                   (version)
import qualified System.Directory                as IO
import           System.Exit                     (exitWith)
import qualified System.IO                       as IO

data Action
  = Validate
  | Reformat

data RunMode
  = ShowVersion
  | Run Config [Extension] Action [FilePath]

-- | Main entry point.
main :: IO ()
main = do
  config <- getConfig
  runMode <-
    execParser
      (info
         (options config <**> helper)
         (header "hindent - Reformat Haskell source code"))
  case runMode of
    ShowVersion -> putStrLn ("hindent " ++ showVersion version)
    Run style exts action paths ->
      if null paths
        then L8.interact
               (either error S.toLazyByteString .
                reformat style (Just exts) Nothing . L8.toStrict)
        else forM_ paths $ \filepath -> do
               cabalexts <- getCabalExtensionsForSourcePath filepath
               text <- S.readFile filepath
               case reformat
                      style
                      (Just $ cabalexts ++ exts)
                      (Just filepath)
                      text of
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

-- | Read config from a config file, or return 'defaultConfig'.
getConfig :: IO Config
getConfig = do
  cur <- Path.getCurrentDir
  homeDir <- Path.getHomeDir
  mfile <-
    Path.findFileUp
      cur
      ((== ".hindent.yaml") . toFilePath . filename)
      (Just homeDir)
  case mfile of
    Nothing -> return defaultConfig
    Just file -> do
      result <- Y.decodeFileEither (toFilePath file)
      case result of
        Left e       -> error (show e)
        Right config -> return config

-- | Program options.
options :: Config -> Parser RunMode
options config =
  flag' ShowVersion (long "version" <> help "Print the version") <|>
  (Run <$> style <*> exts <*> action <*> files)
  where
    style =
      (makeStyle config <$> lineLen <*> indentSpaces <*> trailingNewline <*>
       sortImports) <*
      optional
        (strOption
           (long "style" <>
            help "Style to print with (historical, now ignored)" <>
            metavar "STYLE") :: Parser String)
    exts =
      fmap
        getExtensions
        (many
           (T.pack <$>
            strOption
              (short 'X' <> help "Language extension" <> metavar "GHCEXT")))
    indentSpaces =
      option
        auto
        (long "indent-size" <>
         help "Indentation size in spaces" <>
         value (configIndentSpaces config) <> showDefault) <|>
      option
        auto
        (long "tab-size" <> help "Same as --indent-size, for compatibility")
    lineLen =
      option
        auto
        (long "line-length" <>
         help "Desired length of lines" <>
         value (configMaxColumns config) <> showDefault)
    trailingNewline =
      not <$>
      flag
        (not (configTrailingNewline config))
        (configTrailingNewline config)
        (long "no-force-newline" <>
         help "Don't force a trailing newline" <> showDefault)
    sortImports =
      flag
        Nothing
        (Just True)
        (long "sort-imports" <> help "Sort imports in groups" <> showDefault) <|>
      flag
        Nothing
        (Just False)
        (long "no-sort-imports" <> help "Don't sort imports")
    action =
      flag
        Reformat
        Validate
        (long "validate" <>
         help "Check if files are formatted without changing them")
    makeStyle s mlen tabs trailing imports =
      s
        { configMaxColumns = mlen
        , configIndentSpaces = tabs
        , configTrailingNewline = trailing
        , configSortImports = fromMaybe (configSortImports s) imports
        }
    files = many (strArgument (metavar "FILENAMES"))
