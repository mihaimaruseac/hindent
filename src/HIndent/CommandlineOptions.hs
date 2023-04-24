-- | Types and functions related to HIndent's commandline options.
module HIndent.CommandlineOptions
  ( Action(..)
  , RunMode(..)
  , options
  ) where

import Data.Maybe
import qualified Data.Text as T
import HIndent.Config
import HIndent.LanguageExtension
import HIndent.LanguageExtension.Types
import Options.Applicative hiding (action, style)

-- | HIndent actions.
data Action
  = Validate -- ^ Validate if the code is formatted.
  | Reformat -- ^ Format the code.

-- | HIndent running mode.
data RunMode
  = ShowVersion -- ^ Show HIndent's version.
  | Run Config [Extension] Action [FilePath] -- ^ Format or validate the code.

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
