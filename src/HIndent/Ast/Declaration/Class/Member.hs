module HIndent.Ast.Declaration.Class.Member
  ( ClassMember
  , mkClassSignatureMember
  , mkClassMethodMember
  , mkAssociatedFamilyMember
  , mkAssociatedTypeDefaultMember
  ) where

import HIndent.Ast.Declaration.Bind
import HIndent.Ast.Declaration.Family.Data
import HIndent.Ast.Declaration.Family.Type
import HIndent.Ast.Declaration.Instance.Family.Type.Associated.Default
import HIndent.Ast.Declaration.Signature
import HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.NodeComments

data ClassMember
  = Signature Signature
  | Method Bind
  | AssociatedDataFamily DataFamily
  | AssociatedTypeDefault AssociatedTypeDefault
  | AssociatedTypeFamily TypeFamily

instance CommentExtraction ClassMember where
  nodeComments Signature {} = NodeComments [] [] []
  nodeComments Method {} = NodeComments [] [] []
  nodeComments AssociatedDataFamily {} = NodeComments [] [] []
  nodeComments AssociatedTypeDefault {} = NodeComments [] [] []
  nodeComments AssociatedTypeFamily {} = NodeComments [] [] []

instance Pretty ClassMember where
  pretty (Signature signature) = pretty signature
  pretty (Method bind) = pretty bind
  pretty (AssociatedDataFamily dataFamily) = pretty dataFamily
  pretty (AssociatedTypeDefault associatedTypeDefault) =
    pretty associatedTypeDefault
  pretty (AssociatedTypeFamily typeFamily) = pretty typeFamily

mkClassSignatureMember :: GHC.Sig GHC.GhcPs -> ClassMember
mkClassSignatureMember = Signature . mkSignature

mkClassMethodMember :: GHC.HsBind GHC.GhcPs -> ClassMember
mkClassMethodMember = Method . mkBind

mkAssociatedFamilyMember :: GHC.FamilyDecl GHC.GhcPs -> ClassMember
mkAssociatedFamilyMember familyDecl
  | Just typeFamily <- mkTypeFamily familyDecl = AssociatedTypeFamily typeFamily
  | Just dataFamily <- mkDataFamily familyDecl = AssociatedDataFamily dataFamily
  | otherwise = error "Unreachable"

mkAssociatedTypeDefaultMember :: GHC.TyFamInstDecl GHC.GhcPs -> ClassMember
mkAssociatedTypeDefaultMember = AssociatedTypeDefault . mkAssociatedTypeDefault
