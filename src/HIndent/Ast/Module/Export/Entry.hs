{-# LANGUAGE CPP #-}

module HIndent.Ast.Module.Export.Entry
  ( ExportEntry
  , mkExportEntry
  ) where

import GHC.Stack
import qualified GHC.Types.SrcLoc as GHC
import qualified GHC.Unit as GHC
import HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
data ExportEntry
  = SingleIdentifier (GHC.LIEWrappedName GHC.GhcPs)
  | WithSpecificConstructors
      (GHC.LIEWrappedName GHC.GhcPs)
      [GHC.LIEWrappedName GHC.GhcPs]
  | WithAllConstructors (GHC.LIEWrappedName GHC.GhcPs)
  | ByModule (GHC.GenLocated GHC.SrcSpanAnnA GHC.ModuleName)
#else
data ExportEntry
  = SingleIdentifier (GHC.LIEWrappedName (GHC.IdP GHC.GhcPs))
  | WithSpecificConstructors
      (GHC.LIEWrappedName (GHC.IdP GHC.GhcPs))
      [GHC.LIEWrappedName (GHC.IdP GHC.GhcPs)]
  | WithAllConstructors (GHC.LIEWrappedName (GHC.IdP GHC.GhcPs))
  | ByModule (GHC.GenLocated GHC.SrcSpanAnnA GHC.ModuleName)
#endif
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
mkExportEntry (GHC.IEVar _ name) = SingleIdentifier name
mkExportEntry (GHC.IEThingAbs _ name) = SingleIdentifier name
mkExportEntry (GHC.IEThingAll _ name) = WithAllConstructors name
mkExportEntry (GHC.IEThingWith _ name _ constructors) =
  WithSpecificConstructors name constructors
mkExportEntry (GHC.IEModuleContents _ name) = ByModule name
mkExportEntry GHC.IEGroup {} = neverAppears
mkExportEntry GHC.IEDoc {} = neverAppears
mkExportEntry GHC.IEDocNamed {} = neverAppears

neverAppears :: HasCallStack => a
neverAppears =
  error
    "This AST node should never appear in the GHC AST. If you see this error message, please report a bug to the HIndent maintainers."
