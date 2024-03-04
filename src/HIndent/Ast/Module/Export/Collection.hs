module HIndent.Ast.Module.Export.Collection
  ( ExportCollection
  , mkExportCollection
  ) where

import           HIndent.Ast.Module.Export.Entry
import           HIndent.Ast.NodeComments           hiding (fromEpAnn)
import           HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments

newtype ExportCollection =
  ExportCollection [ExportEntry]

instance CommentExtraction ExportCollection where
  nodeComments (ExportCollection _) = NodeComments [] [] []

instance Pretty ExportCollection where
  pretty' (ExportCollection xs) = vTuple $ fmap pretty xs

mkExportCollection :: GHC.HsModule' -> Maybe (WithComments ExportCollection)
mkExportCollection =
  fmap (fmap (ExportCollection . fmap mkExportEntry) . fromGenLocated) .
  GHC.hsmodExports
