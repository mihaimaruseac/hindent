{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Data.GADT.Constructor.Signature
  ( ConstructorSignature
  , mkConstructorSignature
  , prettyHorizontally
  , prettyVertically
  ) where

import HIndent.Ast.Declaration.Data.Record.Field
import HIndent.Ast.NodeComments
import HIndent.Ast.Type
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Printer

data ConstructorSignature
  = ByArrows (WithComments Type)
  | Record
      { fields :: WithComments [WithComments RecordField]
      , result :: WithComments Type
      }

instance CommentExtraction ConstructorSignature where
  nodeComments (ByArrows _) = NodeComments [] [] []
  nodeComments Record {} = NodeComments [] [] []

prettyHorizontally :: ConstructorSignature -> Printer ()
prettyHorizontally (ByArrows signature) = pretty signature
prettyHorizontally Record {..} =
  inter
    (string " -> ")
    [prettyWith fields (vFields' . fmap pretty), pretty result]

prettyVertically :: ConstructorSignature -> Printer ()
prettyVertically (ByArrows signature) =
  pretty $ fmap mkVerticalFuncType signature
prettyVertically Record {..} =
  prefixedLined
    "-> "
    [prettyWith fields (vFields' . fmap pretty), pretty result]

mkConstructorSignature :: GHC.ConDecl GHC.GhcPs -> Maybe ConstructorSignature
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkConstructorSignature GHC.ConDeclGADT {con_g_args = GHC.PrefixConGADT _ xs, ..} =
  Just $ ByArrows (buildFunctionType xs con_res_ty)
#else
mkConstructorSignature GHC.ConDeclGADT {con_g_args = GHC.PrefixConGADT xs, ..} =
  Just $ ByArrows (buildFunctionType xs con_res_ty)
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkConstructorSignature GHC.ConDeclGADT {con_g_args = GHC.RecConGADT _ xs, ..} =
  Just
    $ Record
        { fields =
            fromGenLocated
              $ fmap (fmap (fmap mkRecordField . fromGenLocated)) xs
        , result = mkType <$> fromGenLocated con_res_ty
        }
#elif MIN_VERSION_ghc_lib_parser(9, 4, 0)
mkConstructorSignature GHC.ConDeclGADT {con_g_args = GHC.RecConGADT xs _, ..} =
  Just
    $ Record
        { fields =
            fromGenLocated
              $ fmap (fmap (fmap mkRecordField . fromGenLocated)) xs
        , result = mkType <$> fromGenLocated con_res_ty
        }
#else
mkConstructorSignature GHC.ConDeclGADT {con_g_args = GHC.RecConGADT xs, ..} =
  Just
    $ Record
        { fields =
            fromGenLocated
              $ fmap (fmap (fmap mkRecordField . fromGenLocated)) xs
        , result = mkType <$> fromGenLocated con_res_ty
        }
#endif
mkConstructorSignature GHC.ConDeclH98 {} = Nothing
#if MIN_VERSION_ghc_lib_parser(9, 14, 0)
buildFunctionType ::
     [GHC.HsConDeclField GHC.GhcPs]
  -> GHC.LHsType GHC.GhcPs
  -> WithComments Type
buildFunctionType fields resultTy =
  mkType <$> fromGenLocated (foldr mkFun resultTy fields)

mkFun ::
     GHC.HsConDeclField GHC.GhcPs
  -> GHC.LHsType GHC.GhcPs
  -> GHC.LHsType GHC.GhcPs
mkFun field accTy =
  GHC.noLocA
    (GHC.HsFunTy
       GHC.noExtField
       (GHC.cdf_multiplicity field)
       (GHC.cdf_type field)
       accTy)
#else
buildFunctionType ::
     [GHC.HsScaled GHC.GhcPs (GHC.LHsType GHC.GhcPs)]
  -> GHC.LHsType GHC.GhcPs
  -> WithComments Type
buildFunctionType scaledTypes resultTy =
  mkType <$> fromGenLocated (foldr mkFun resultTy scaledTypes)

mkFun ::
     GHC.HsScaled GHC.GhcPs (GHC.LHsType GHC.GhcPs)
  -> GHC.LHsType GHC.GhcPs
  -> GHC.LHsType GHC.GhcPs
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkFun (GHC.HsScaled mult paramTy) accTy =
  GHC.noLocA (GHC.HsFunTy GHC.noExtField mult paramTy accTy)
#elif MIN_VERSION_ghc_lib_parser(9, 4, 0)
mkFun (GHC.HsScaled mult paramTy) accTy =
  GHC.noLocA (GHC.HsFunTy GHC.noAnn mult paramTy accTy)
#else
mkFun (GHC.HsScaled mult paramTy) accTy =
  GHC.noLocA (GHC.HsFunTy GHC.NoExtField mult paramTy accTy)
#endif
#endif
