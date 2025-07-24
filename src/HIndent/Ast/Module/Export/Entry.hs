{-# LANGUAGE CPP #-}

module HIndent.Ast.Module.Export.Entry
  ( ExportEntry
  , mkExportEntry
  ) where

import GHC.Stack
import HIndent.Ast.Module.Name
import HIndent.Ast.Name.ImportExport
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data ExportEntry
  = SingleIdentifier (WithComments ImportExportName)
  | WithSpecificConstructors
      (WithComments ImportExportName)
      [WithComments ImportExportName]
  | WithAllConstructors (WithComments ImportExportName)
  | ByModule (WithComments ModuleName)

instance CommentExtraction ExportEntry where
  nodeComments SingleIdentifier {} = NodeComments [] [] []
  nodeComments WithSpecificConstructors {} = NodeComments [] [] []
  nodeComments WithAllConstructors {} = NodeComments [] [] []
  nodeComments ByModule {} = NodeComments [] [] []

instance Pretty ExportEntry where
  pretty' (SingleIdentifier s) = pretty s
  pretty' (WithSpecificConstructors s xs) = pretty s >> hTuple (fmap pretty xs)
  pretty' (WithAllConstructors s) = pretty s >> string "(..)"
  pretty' (ByModule s) = string "module " >> pretty s

mkExportEntry :: GHC.IE GHC.GhcPs -> ExportEntry
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkExportEntry (GHC.IEVar _ name _) =
  SingleIdentifier $ mkImportExportName <$> fromGenLocated name
mkExportEntry (GHC.IEThingAbs _ name _) =
  SingleIdentifier $ mkImportExportName <$> fromGenLocated name
mkExportEntry (GHC.IEThingAll _ name _) =
  WithAllConstructors $ mkImportExportName <$> fromGenLocated name
mkExportEntry (GHC.IEThingWith _ name _ constructors _) =
  WithSpecificConstructors (mkImportExportName <$> fromGenLocated name)
    $ fmap mkImportExportName . fromGenLocated <$> constructors
#else
mkExportEntry (GHC.IEVar _ name) =
  SingleIdentifier $ mkImportExportName <$> fromGenLocated name
mkExportEntry (GHC.IEThingAbs _ name) =
  SingleIdentifier $ mkImportExportName <$> fromGenLocated name
mkExportEntry (GHC.IEThingAll _ name) =
  WithAllConstructors $ mkImportExportName <$> fromGenLocated name
mkExportEntry (GHC.IEThingWith _ name _ constructors) =
  WithSpecificConstructors (mkImportExportName <$> fromGenLocated name)
    $ fmap mkImportExportName . fromGenLocated <$> constructors
#endif
mkExportEntry (GHC.IEModuleContents _ name) =
  ByModule $ mkModuleName <$> fromGenLocated name
mkExportEntry GHC.IEGroup {} = neverAppears
mkExportEntry GHC.IEDoc {} = neverAppears
mkExportEntry GHC.IEDocNamed {} = neverAppears

neverAppears :: HasCallStack => a
neverAppears =
  error
    "This AST node should never appear in the GHC AST. If you see this error message, please report a bug to the HIndent maintainers."
