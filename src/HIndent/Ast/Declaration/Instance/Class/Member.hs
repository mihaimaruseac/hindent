module HIndent.Ast.Declaration.Instance.Class.Member
  ( ClassInstanceMember
  , mkAssociatedDataInstanceMember
  , mkAssociatedTypeInstanceMember
  , mkClassInstanceMethodMember
  , mkClassInstanceSignatureMember
  ) where

import HIndent.Ast.Declaration.Bind
import HIndent.Ast.Declaration.Instance.Family.Data.Associated
import HIndent.Ast.Declaration.Instance.Family.Type.Associated
import HIndent.Ast.Declaration.Signature
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty

data ClassInstanceMember
  = AssociatedDataInstance AssociatedDataFamilyInstance
  | AssociatedTypeInstance AssociatedType
  | Method Bind
  | Signature Signature

instance Pretty ClassInstanceMember where
  pretty (AssociatedDataInstance associatedDataInstance) =
    pretty associatedDataInstance
  pretty (AssociatedTypeInstance associatedTypeInstance) =
    pretty associatedTypeInstance
  pretty (Method bind) = pretty bind
  pretty (Signature signature) = pretty signature

mkAssociatedDataInstanceMember ::
     GHC.DataFamInstDecl GHC.GhcPs -> ClassInstanceMember
mkAssociatedDataInstanceMember =
  AssociatedDataInstance . mkAssociatedDataFamilyInstance

mkAssociatedTypeInstanceMember ::
     GHC.TyFamInstDecl GHC.GhcPs -> ClassInstanceMember
mkAssociatedTypeInstanceMember = AssociatedTypeInstance . mkAssociatedType

mkClassInstanceMethodMember :: GHC.HsBind GHC.GhcPs -> ClassInstanceMember
mkClassInstanceMethodMember = Method . mkBind

mkClassInstanceSignatureMember :: GHC.Sig GHC.GhcPs -> ClassInstanceMember
mkClassInstanceSignatureMember = Signature . mkSignature
