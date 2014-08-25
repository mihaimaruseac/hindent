-- | Haskell indenter.

module HIndent where

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
reformat :: Text -> Either String Builder
reformat x =
  case parseDeclWithMode parseMode
                         (T.unpack x) of
    ParseOk v -> Right (prettyPrint v)
    ParseFailed _ e -> Left e

-- | Pretty print the given printable thing.
prettyPrint :: Pretty a => a -> Builder
prettyPrint v =
  psOutput (execState (runPrinter (pretty v))
                      (PrintState 0
                                  mempty
                                  False
                                  0))

-- | Parse mode, includes all extensions, doesn't assume any fixities.
parseMode :: ParseMode
parseMode =
  defaultParseMode {extensions = allExtensions
                   ,fixities = Nothing}
  where allExtensions =
          filter isDisabledExtention
                 knownExtensions
        isDisabledExtention (DisableExtension _) =
          False
        isDisabledExtention _ =
          True

test :: Text -> IO ()
test =
  either error
         (T.putStrLn .
          T.toLazyText) .
  reformat
