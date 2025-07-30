{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Foreign
  ( ForeignDeclaration
  , mkForeignDeclaration
  ) where

import Data.Maybe
import qualified GHC.Types.ForeignCall as GHC
import qualified GHC.Types.SourceText as GHC
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.Declaration.Foreign.CallingConvention
import HIndent.Ast.Declaration.Foreign.Safety
import HIndent.Ast.Name.Prefix
import HIndent.Ast.NodeComments
import HIndent.Ast.Type (Type, mkTypeFromHsSigType)
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 8, 0)
import qualified GHC.Data.FastString as GHC
#endif
data ForeignDeclaration
  = ForeignImport
      { convention :: CallingConvention
      , safety :: Safety
      , srcIdent :: Maybe String
      , dstIdent :: WithComments PrefixName
      , signature :: WithComments Type
      }
  | ForeignExport
      { convention :: CallingConvention
      , srcIdent :: Maybe String
      , dstIdent :: WithComments PrefixName
      , signature :: WithComments Type
      }

instance CommentExtraction ForeignDeclaration where
  nodeComments ForeignImport {} = NodeComments [] [] []
  nodeComments ForeignExport {} = NodeComments [] [] []

instance Pretty ForeignDeclaration where
  pretty' ForeignImport {..} =
    spaced
      $ [string "foreign import", pretty convention, pretty safety]
          ++ maybeToList (fmap string srcIdent)
          ++ [pretty dstIdent, string "::", pretty signature]
  pretty' ForeignExport {..} =
    spaced
      $ [string "foreign export", pretty convention]
          ++ maybeToList (fmap string srcIdent)
          ++ [pretty dstIdent, string "::", pretty signature]

mkForeignDeclaration :: GHC.ForeignDecl GHC.GhcPs -> ForeignDeclaration
#if MIN_VERSION_ghc_lib_parser(9, 8, 0)
mkForeignDeclaration GHC.ForeignImport { fd_fi = (GHC.CImport (GHC.L _ src) (GHC.L _ conv) (GHC.L _ sfty) _ _)
                                       , ..
                                       } = ForeignImport {..}
  where
    convention = mkCallingConvention conv
    safety = mkSafety sfty
    srcIdent =
      case src of
        GHC.SourceText s -> Just $ GHC.unpackFS s
        _ -> Nothing
    dstIdent = fromGenLocated $ fmap mkPrefixName fd_name
    signature =
      flattenComments $ mkTypeFromHsSigType <$> fromGenLocated fd_sig_ty
mkForeignDeclaration GHC.ForeignExport { fd_fe = (GHC.CExport (GHC.L _ src) (GHC.L _ (GHC.CExportStatic _ _ conv)))
                                       , ..
                                       } = ForeignExport {..}
  where
    convention = mkCallingConvention conv
    srcIdent =
      case src of
        GHC.SourceText s -> Just $ GHC.unpackFS s
        _ -> Nothing
    dstIdent = fromGenLocated $ fmap mkPrefixName fd_name
    signature =
      flattenComments $ mkTypeFromHsSigType <$> fromGenLocated fd_sig_ty
#elif MIN_VERSION_ghc_lib_parser(9, 6, 0)
mkForeignDeclaration GHC.ForeignImport { fd_fi = (GHC.CImport (GHC.L _ src) (GHC.L _ conv) (GHC.L _ sfty) _ _)
                                       , ..
                                       } = ForeignImport {..}
  where
    convention = mkCallingConvention conv
    safety = mkSafety sfty
    srcIdent =
      case src of
        GHC.SourceText s -> Just s
        _ -> Nothing
    dstIdent = fromGenLocated $ fmap mkPrefixName fd_name
    signature =
      flattenComments $ mkTypeFromHsSigType <$> fromGenLocated fd_sig_ty
mkForeignDeclaration GHC.ForeignExport { fd_fe = (GHC.CExport (GHC.L _ src) (GHC.L _ (GHC.CExportStatic _ _ conv)))
                                       , ..
                                       } = ForeignExport {..}
  where
    convention = mkCallingConvention conv
    srcIdent =
      case src of
        GHC.SourceText s -> Just s
        _ -> Nothing
    dstIdent = fromGenLocated $ fmap mkPrefixName fd_name
    signature =
      flattenComments $ mkTypeFromHsSigType <$> fromGenLocated fd_sig_ty
#else
mkForeignDeclaration GHC.ForeignImport { fd_fi = (GHC.CImport (GHC.L _ conv) (GHC.L _ sfty) _ _ (GHC.L _ src))
                                       , ..
                                       } = ForeignImport {..}
  where
    convention = mkCallingConvention conv
    safety = mkSafety sfty
    srcIdent =
      case src of
        GHC.SourceText s -> Just s
        _ -> Nothing
    dstIdent = fromGenLocated $ fmap mkPrefixName fd_name
    signature =
      flattenComments $ mkTypeFromHsSigType <$> fromGenLocated fd_sig_ty
mkForeignDeclaration GHC.ForeignExport { fd_fe = (GHC.CExport (GHC.L _ (GHC.CExportStatic _ _ conv)) (GHC.L _ src))
                                       , ..
                                       } = ForeignExport {..}
  where
    convention = mkCallingConvention conv
    srcIdent =
      case src of
        GHC.SourceText s -> Just s
        _ -> Nothing
    dstIdent = fromGenLocated $ fmap mkPrefixName fd_name
    signature =
      flattenComments $ mkTypeFromHsSigType <$> fromGenLocated fd_sig_ty
#endif
