-- | Haskell indenter.

module HIndent where

import           HIndent.Instances ()
import           HIndent.Types

import           Control.Monad.State
import           Data.Monoid
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.IO as TIO
import           Language.Haskell.Exts.Parser

-- | Format the string and print it to stdout. Throws exception on
-- invalid syntax.
indent :: String -> IO ()
indent x =
  case reformat x of
    Right b -> TIO.putStrLn (T.toLazyText b)
    Left e -> error e

-- | Format the given source.
reformat :: String -> Either String Builder
reformat x =
  case parseExp x of
    ParseOk v ->
      Right (prettyPrint v)
    ParseFailed _ e ->
      Left e

-- | Pretty print the given printable thing.
prettyPrint :: Pretty a => a -> Builder
prettyPrint v =
  psOutput
    (execState (runPrinter (pretty v))
               (PrintState 0 mempty False 0))
