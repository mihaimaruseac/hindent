module HIndent.Ast.Module.Export.Entry
  ( ExportEntry
  , mkExportEntry
  ) where

import           HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import           HIndent.Pretty
import           HIndent.Pretty.NodeComments

newtype ExportEntry =
  ExportEntry (GHC.LIE GHC.GhcPs)

instance CommentExtraction ExportEntry where
  nodeComments (ExportEntry _) = NodeComments [] [] []

instance Pretty ExportEntry where
  pretty' (ExportEntry x) = pretty x

mkExportEntry :: GHC.LIE GHC.GhcPs -> ExportEntry
mkExportEntry = ExportEntry
