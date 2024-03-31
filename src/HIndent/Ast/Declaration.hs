{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration
  ( Declaration(..)
  , mkDeclaration
  , isSignature
  ) where

import Control.Applicative
import Data.Maybe
import HIndent.Ast.Declaration.Bind
import HIndent.Ast.Declaration.Class
import HIndent.Ast.Declaration.Data
import HIndent.Ast.Declaration.Family.Data
import HIndent.Ast.Declaration.Family.Type
import HIndent.Ast.Declaration.Instance.Class
import HIndent.Ast.Declaration.Instance.Family.Data
import HIndent.Ast.Declaration.Instance.Family.Type
import HIndent.Ast.Declaration.Signature
import HIndent.Ast.Declaration.StandAloneDeriving
import HIndent.Ast.Declaration.TypeSynonym
import HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.NodeComments

data Declaration
  = DataFamily DataFamily
  | TypeFamily TypeFamily
  | DataDeclaration DataDeclaration
  | ClassDeclaration ClassDeclaration
  | TypeSynonym TypeSynonym
  | ClassInstance ClassInstance
  | DataFamilyInstance DataFamilyInstance
  | TypeFamilyInstance TypeFamilyInstance
  | StandAloneDeriving StandAloneDeriving
  | Bind Bind
  | Signature Signature
  | KindSigDecl (GHC.StandaloneKindSig GHC.GhcPs)
  | DefDecl (GHC.DefaultDecl GHC.GhcPs)
  | ForDecl (GHC.ForeignDecl GHC.GhcPs)
  | WarningDecl (GHC.WarnDecls GHC.GhcPs)
  | AnnDecl (GHC.AnnDecl GHC.GhcPs)
  | RuleDecl (GHC.RuleDecls GHC.GhcPs)
  | SpliceDecl (GHC.SpliceDecl GHC.GhcPs)
  | RoleAnnotDecl (GHC.RoleAnnotDecl GHC.GhcPs)

instance CommentExtraction Declaration where
  nodeComments DataFamily {} = NodeComments [] [] []
  nodeComments TypeFamily {} = NodeComments [] [] []
  nodeComments DataDeclaration {} = NodeComments [] [] []
  nodeComments ClassDeclaration {} = NodeComments [] [] []
  nodeComments TypeSynonym {} = NodeComments [] [] []
  nodeComments ClassInstance {} = NodeComments [] [] []
  nodeComments DataFamilyInstance {} = NodeComments [] [] []
  nodeComments TypeFamilyInstance {} = NodeComments [] [] []
  nodeComments StandAloneDeriving {} = NodeComments [] [] []
  nodeComments Bind {} = NodeComments [] [] []
  nodeComments Signature {} = NodeComments [] [] []
  nodeComments KindSigDecl {} = NodeComments [] [] []
  nodeComments DefDecl {} = NodeComments [] [] []
  nodeComments ForDecl {} = NodeComments [] [] []
  nodeComments WarningDecl {} = NodeComments [] [] []
  nodeComments AnnDecl {} = NodeComments [] [] []
  nodeComments RuleDecl {} = NodeComments [] [] []
  nodeComments SpliceDecl {} = NodeComments [] [] []
  nodeComments RoleAnnotDecl {} = NodeComments [] [] []

instance Pretty Declaration where
  pretty' (DataFamily x) = pretty x
  pretty' (TypeFamily x) = pretty x
  pretty' (DataDeclaration x) = pretty x
  pretty' (ClassDeclaration x) = pretty x
  pretty' (TypeSynonym x) = pretty x
  pretty' (ClassInstance x) = pretty x
  pretty' (DataFamilyInstance x) = pretty x
  pretty' (TypeFamilyInstance x) = pretty x
  pretty' (StandAloneDeriving x) = pretty x
  pretty' (Bind x) = pretty x
  pretty' (Signature x) = pretty x
  pretty' (KindSigDecl x) = pretty x
  pretty' (DefDecl x) = pretty x
  pretty' (ForDecl x) = pretty x
  pretty' (WarningDecl x) = pretty x
  pretty' (AnnDecl x) = pretty x
  pretty' (RuleDecl x) = pretty x
  pretty' (SpliceDecl x) = pretty x
  pretty' (RoleAnnotDecl x) = pretty x

mkDeclaration :: GHC.HsDecl GHC.GhcPs -> Declaration
mkDeclaration (GHC.TyClD _ (GHC.FamDecl _ x)) =
  fromMaybe (error "Unreachable.")
    $ DataFamily <$> mkDataFamily x <|> TypeFamily <$> mkTypeFamily x
mkDeclaration (GHC.TyClD _ x@GHC.SynDecl {}) = TypeSynonym $ mkTypeSynonym x
mkDeclaration (GHC.TyClD _ x@GHC.DataDecl {}) =
  maybe (error "Unreachable.") DataDeclaration (mkDataDeclaration x)
mkDeclaration (GHC.TyClD _ x@GHC.ClassDecl {}) =
  maybe (error "Unreachable.") ClassDeclaration (mkClassDeclaration x)
mkDeclaration (GHC.InstD _ x@GHC.ClsInstD {}) =
  maybe (error "Unreachable.") ClassInstance (mkClassInstance x)
mkDeclaration (GHC.InstD _ GHC.DataFamInstD {GHC.dfid_inst = GHC.DataFamInstDecl {..}}) =
  DataFamilyInstance $ mkDataFamilyInstance dfid_eqn
mkDeclaration (GHC.InstD _ x@GHC.TyFamInstD {}) =
  maybe (error "Unreachable.") TypeFamilyInstance $ mkTypeFamilyInstance x
mkDeclaration (GHC.DerivD _ x) = StandAloneDeriving $ mkStandAloneDeriving x
mkDeclaration (GHC.ValD _ x) = Bind $ mkBind x
mkDeclaration (GHC.SigD _ x) = Signature $ mkSignature x
mkDeclaration (GHC.KindSigD _ x) = KindSigDecl x
mkDeclaration (GHC.DefD _ x) = DefDecl x
mkDeclaration (GHC.ForD _ x) = ForDecl x
mkDeclaration (GHC.WarningD _ x) = WarningDecl x
mkDeclaration (GHC.AnnD _ x) = AnnDecl x
mkDeclaration (GHC.RuleD _ x) = RuleDecl x
mkDeclaration (GHC.SpliceD _ x) = SpliceDecl x
mkDeclaration (GHC.RoleAnnotD _ x) = RoleAnnotDecl x
mkDeclaration GHC.DocD {} =
  error
    "This node should never appear in the AST. If you see this error, please report it to the HIndent maintainers."

isSignature :: Declaration -> Bool
isSignature Signature {} = True
isSignature _ = False
