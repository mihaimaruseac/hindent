-- | Printer combinators for handling comments.
module HIndent.Pretty.Combinators.Comment
  ( eolCommentsArePrinted
  ) where

import           Control.Monad.State
import           HIndent.Types

-- | Claims that comments were printed. Next time calling 'string' will
-- print a newline before printing a text.
eolCommentsArePrinted :: Printer ()
eolCommentsArePrinted = modify (\s -> s {psEolComment = True})
