module HIndent.Ast.Import.Entry
  ( ImportEntry(..)
  , mkImportEntry
  ) where

import qualified GHC.Hs                      as GHC
import           HIndent.Ast.NodeComments
import           HIndent.Pretty
import           HIndent.Pretty.NodeComments

newtype ImportEntry = ImportEntry
  { name :: GHC.IE GHC.GhcPs
  }

instance CommentExtraction ImportEntry where
  nodeComments _ = NodeComments [] [] []

instance Pretty ImportEntry where
  pretty' = pretty . name

mkImportEntry :: GHC.IE GHC.GhcPs -> ImportEntry
mkImportEntry = ImportEntry
