module HIndent.Ast.Module.Export.Entry
  ( ExportEntry
  , mkExportEntry
  ) where

import           HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments

data ExportEntry
  = SingleIdentifier String
  | WithSpecificConstructors String [String]
  | WithAllConstructors String
  | ByModule String

instance CommentExtraction ExportEntry where
  nodeComments SingleIdentifier {}         = NodeComments [] [] []
  nodeComments WithSpecificConstructors {} = NodeComments [] [] []
  nodeComments WithAllConstructors {}      = NodeComments [] [] []
  nodeComments ByModule {}                 = NodeComments [] [] []

instance Pretty ExportEntry where
  pretty' (SingleIdentifier s)            = string s
  pretty' (WithSpecificConstructors s xs) = string s >> hTuple (fmap string xs)
  pretty' (WithAllConstructors s)         = string s >> string "(..)"
  pretty' (ByModule s)                    = string "module " >> string s

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
mkExportEntry GHC.IEGroup {} = undefined
mkExportEntry GHC.IEDoc {} = undefined
mkExportEntry GHC.IEDocNamed {} = undefined
