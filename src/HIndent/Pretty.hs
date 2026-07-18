-- | Pretty printing.
module HIndent.Pretty
  ( Pretty(..)
  ) where

import HIndent.Printer

-- | Pretty print.
class Pretty a where
  pretty :: a -> Printer ()
