-- | Haskell indenter.

module HIndent
  (-- * Formatting functions.
   reformat
  ,prettyPrint
  ,parseMode
  -- * Style
  ,styles
  ,chrisdone
  ,fundamental
  -- * Testing
  ,test)
  where

import           HIndent.Styles.ChrisDone
import           HIndent.Styles.Fundamental
import           HIndent.Instances ()
import           HIndent.Types

import           Control.Monad.State
import           Data.Monoid
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as T
import           Language.Haskell.Exts.Extension
import           Language.Haskell.Exts.Parser

-- | Format the given source.
reformat :: Style -> Text -> Either String Builder
reformat style x =
  case parseDeclWithComments parseMode
                             (T.unpack x) of
    ParseOk (v,_comments) -> Right (prettyPrint style v)
    ParseFailed _ e -> Left e

-- | Pretty print the given printable thing.
prettyPrint :: Pretty a => Style -> a -> Builder
prettyPrint style a =
  psOutput (execState (runPrinter (pretty a))
                      (case style of
                         Style _name _author _desc st extenders ->
                           PrintState 0
                                      mempty
                                      False
                                      0
                                      1
                                      st
                                      extenders))

-- | Parse mode, includes all extensions, doesn't assume any fixities.
parseMode :: ParseMode
parseMode =
  defaultParseMode {extensions = allExtensions
                   ,fixities = Nothing}
  where allExtensions =
          filter isDisabledExtention knownExtensions
        isDisabledExtention (DisableExtension _) = False
        isDisabledExtention _ = True

-- | Test with the given style, prints to stdout.
test :: Style -> Text -> IO ()
test style =
  either error (T.putStrLn . T.toLazyText) .
  reformat style

-- | Styles list, useful for programmatically choosing.
styles :: [Style]
styles = [fundamental,chrisdone]
