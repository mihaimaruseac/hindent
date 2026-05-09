module HIndent.Ast.Module.Export.Collection
  ( ExportCollection
  , mkExportCollection
  ) where

import HIndent.Ast.Module.Export.Entry
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators

newtype ExportCollection =
  ExportCollection [WithComments ExportEntry]

instance Pretty ExportCollection where
  pretty (ExportCollection xs) = vTuple $ fmap pretty xs

mkExportCollection :: GHC.HsModule' -> Maybe (WithComments ExportCollection)
mkExportCollection =
  fmap
    (fmap
       (ExportCollection
          . fmap (fmap mkExportEntry . mkWithCommentsFromGenLocated))
       . mkWithCommentsFromGenLocated)
    . GHC.hsmodExports
