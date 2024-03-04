module HIndent.Ast.Module.Export.Collection
  ( ExportCollection
  , mkExportCollection
  ) where

import           HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments

newtype ExportCollection =
  ExportCollection (GHC.LocatedL [GHC.LIE GHC.GhcPs])

instance CommentExtraction ExportCollection where
  nodeComments (ExportCollection _) = NodeComments [] [] []

instance Pretty ExportCollection where
  pretty' (ExportCollection xs) = printCommentsAnd xs (vTuple . fmap pretty)

mkExportCollection :: GHC.HsModule' -> Maybe ExportCollection
mkExportCollection = fmap ExportCollection . GHC.hsmodExports
