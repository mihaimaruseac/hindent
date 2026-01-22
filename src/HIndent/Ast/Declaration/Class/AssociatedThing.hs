{-# LANGUAGE CPP #-}

module HIndent.Ast.Declaration.Class.AssociatedThing
  ( ClassAssociatedThing
  , LClassAssociatedThing
  , mkClassAssociatedSig
  , mkClassAssociatedMethod
  , mkClassAssociatedFamily
  , mkClassAssociatedTypeDefault
  , foldClassAssociatedThing
  ) where

import Data.Maybe (fromMaybe)
import HIndent.Ast.Declaration.Bind
import HIndent.Ast.Declaration.Family.Data
import HIndent.Ast.Declaration.Family.Type
import HIndent.Ast.Declaration.Instance.Family.Type.Associated.Default
import HIndent.Ast.Declaration.Signature
import HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty)
import HIndent.Pretty.NodeComments

data ClassAssociatedThing
  = Sig (GHC.Sig GHC.GhcPs)
  | Method (GHC.HsBindLR GHC.GhcPs GHC.GhcPs)
  | TypeFam (GHC.FamilyDecl GHC.GhcPs)
  | DataFam (GHC.FamilyDecl GHC.GhcPs)
  | TypeDefault (GHC.TyFamInstDecl GHC.GhcPs)

type LClassAssociatedThing = GHC.LocatedA ClassAssociatedThing

instance CommentExtraction ClassAssociatedThing where
  nodeComments Sig {} = NodeComments [] [] []
  nodeComments Method {} = NodeComments [] [] []
  nodeComments TypeFam {} = NodeComments [] [] []
  nodeComments DataFam {} = NodeComments [] [] []
  nodeComments TypeDefault {} = NodeComments [] [] []

instance Pretty ClassAssociatedThing where
  pretty' (Sig sig) = pretty $ mkSignature sig
  pretty' (Method bind) = pretty $ mkBind bind
  pretty' (TypeFam fam) = pretty $ typeFamilyFrom fam
  pretty' (DataFam fam) = pretty $ dataFamilyFrom fam
  pretty' (TypeDefault defn) = pretty $ mkAssociatedTypeDefault defn

mkClassAssociatedSig :: GHC.Sig GHC.GhcPs -> ClassAssociatedThing
mkClassAssociatedSig = Sig

mkClassAssociatedMethod ::
     GHC.HsBindLR GHC.GhcPs GHC.GhcPs -> ClassAssociatedThing
mkClassAssociatedMethod = Method

mkClassAssociatedFamily :: GHC.FamilyDecl GHC.GhcPs -> ClassAssociatedThing
mkClassAssociatedFamily decl =
  case GHC.fdInfo decl of
    GHC.DataFamily -> DataFam decl
    _ -> TypeFam decl

mkClassAssociatedTypeDefault ::
     GHC.TyFamInstDecl GHC.GhcPs -> ClassAssociatedThing
mkClassAssociatedTypeDefault = TypeDefault

foldClassAssociatedThing ::
     (GHC.Sig GHC.GhcPs -> r)
  -> (GHC.HsBindLR GHC.GhcPs GHC.GhcPs -> r)
  -> (GHC.FamilyDecl GHC.GhcPs -> r)
  -> (GHC.FamilyDecl GHC.GhcPs -> r)
  -> (GHC.TyFamInstDecl GHC.GhcPs -> r)
  -> ClassAssociatedThing
  -> r
foldClassAssociatedThing onSig _ _ _ _ (Sig sig) = onSig sig
foldClassAssociatedThing _ onBind _ _ _ (Method bind) = onBind bind
foldClassAssociatedThing _ _ onType _ _ (TypeFam fam) = onType fam
foldClassAssociatedThing _ _ _ onData _ (DataFam fam) = onData fam
foldClassAssociatedThing _ _ _ _ onDefault (TypeDefault inst) = onDefault inst

typeFamilyFrom :: GHC.FamilyDecl GHC.GhcPs -> TypeFamily
typeFamilyFrom decl = fromMaybe (error "Unreachable") $ mkTypeFamily decl

dataFamilyFrom :: GHC.FamilyDecl GHC.GhcPs -> DataFamily
dataFamilyFrom decl = fromMaybe (error "Unreachable") $ mkDataFamily decl
