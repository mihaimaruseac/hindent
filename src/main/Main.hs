{-# LANGUAGE Unsafe #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Main entry point to hindent.
--
-- hindent

module Main where

import           Control.Applicative
import           Control.Exception
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as S
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Version (showVersion)
import           Descriptive
import           Descriptive.Options
import           Foreign.C.Error
import           GHC.IO.Exception
import           HIndent
import           HIndent.Types
import           Language.Haskell.Exts hiding (Style,style)
import           Paths_hindent (version)
import           System.Directory
import           System.Environment
import           System.IO
import           Text.Read

-- | Main entry point.
main :: IO ()
main = do
    args <- getArgs
    case consume options (map T.pack args) of
        Succeeded (style, exts, mfilepath) ->
            case mfilepath of
                Just filepath -> do
                    text <- S.readFile filepath
                    tmpDir <- getTemporaryDirectory
                    (fp, h) <- openTempFile tmpDir "hindent.hs"
                    case reformat style (Just exts) text of
                        Left e -> error (filepath ++ ": " ++ e)
                        Right out -> do
                            L8.hPutStr h (S.toLazyByteString out)
                            hFlush h
                            hClose h
                            let exdev e =
                                    if ioe_errno e ==
                                       Just ((\(Errno a) -> a) eXDEV)
                                        then copyFile fp filepath >>
                                             removeFile fp
                                        else throw e
                            renameFile fp filepath `catch` exdev
                Nothing ->
                    L8.interact
                        (either error S.toLazyByteString .
                         reformat style (Just exts) . L8.toStrict)
        Failed (Wrap (Stopped Version) _) ->
            putStrLn ("hindent " ++ showVersion version)
        Failed (Wrap (Stopped Help) _) -> putStrLn help
        _ -> error help
  where



help :: [Char]
help =
    "hindent " ++
    T.unpack (textDescription (describe options [])) ++
    "\nVersion " ++ showVersion version ++ "\n" ++
    "-X to pass extensions e.g. -XMagicHash etc.\n" ++
    "The --style option is now ignored, but preserved for backwards-compatibility.\n" ++
    "Johan Tibell is the default and only style."

-- | Options that stop the argument parser.
data Stoppers = Version | Help
  deriving (Show)

-- | Program options.
options :: Monad m
        => Consumer [Text] (Option Stoppers) m (Config,[Extension],Maybe FilePath)
options = ver *> ((,,) <$> style <*> exts <*> file)
  where
    ver =
        stop (flag "version" "Print the version" Version) *>
        stop (flag "help" "Show help" Help)
    style =
        makeStyle <$>
        fmap
            (const defaultConfig)
            (optional
                 (constant "--style" "Style to print with" () *>
                  anyString "STYLE")) <*>
        lineLen <*>
        tabsize <*>
        trailingNewline
    exts = fmap getExtensions (many (prefix "X" "Language extension"))
    tabsize =
        fmap
            (>>= (readMaybe . T.unpack))
            (optional (arg "tab-size" "Tab size, default: 4"))
    lineLen =
        fmap
            (>>= (readMaybe . T.unpack))
            (optional (arg "line-length" "Desired length of lines"))
    trailingNewline =
        optional
            (constant "--no-force-newline" "Don't force a trailing newline" False)
    makeStyle s mlen tabs trailing =
        s
        { configMaxColumns = fromMaybe (configMaxColumns s) mlen
        , configIndentSpaces = fromMaybe (configIndentSpaces s) tabs
        , configTrailingNewline = fromMaybe (configTrailingNewline s) trailing
        }
    file = fmap (fmap T.unpack) (optional (anyString "[<filename>]"))
