module HIndent.Ast.Module.Export.Entry
  ( ExportEntry
  , mkExportEntry
  ) where

import GHC.Stack
import HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data ExportEntry
  = SingleIdentifier String
  | WithSpecificConstructors String [String]
  | WithAllConstructors String
  | ByModule String

instance CommentExtraction ExportEntry where
  nodeComments SingleIdentifier {} = NodeComments [] [] []
  nodeComments WithSpecificConstructors {} = NodeComments [] [] []
  nodeComments WithAllConstructors {} = NodeComments [] [] []
  nodeComments ByModule {} = NodeComments [] [] []

instance Pretty ExportEntry where
  pretty' (SingleIdentifier s) = string s
  pretty' (WithSpecificConstructors s xs) = string s >> hTuple (fmap string xs)
  pretty' (WithAllConstructors s) = string s >> string "(..)"
  pretty' (ByModule s) = string "module " >> string s

mkExportEntry :: GHC.IE GHC.GhcPs -> ExportEntry
mkExportEntry (GHC.IEVar GHC.NoExtField name) =
  SingleIdentifier $ showOutputable name
mkExportEntry (GHC.IEThingAbs _ name) = SingleIdentifier $ showOutputable name
mkExportEntry (GHC.IEThingAll _ name) =
  WithAllConstructors $ showOutputable name
mkExportEntry (GHC.IEThingWith _ name _ constructors) =
  WithSpecificConstructors
    (showOutputable name)
    (fmap showOutputable constructors)
mkExportEntry (GHC.IEModuleContents _ name) = ByModule $ showOutputable name
mkExportEntry GHC.IEGroup {} = neverAppears
mkExportEntry GHC.IEDoc {} = neverAppears
mkExportEntry GHC.IEDocNamed {} = neverAppears

neverAppears :: HasCallStack => a
neverAppears =
  error
    "This AST node should never appear in the GHC AST. If you see this error message, please report a bug to the HIndent maintainers."
