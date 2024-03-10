module HIndent.Ast.Declaration
  ( Declaration(..)
  , mkDeclaration
  , isSignature
  ) where

import           HIndent.Ast.Declaration.Family.Data
import           HIndent.Ast.Declaration.Family.Type
import           HIndent.Ast.Declaration.Instance.Class
import           HIndent.Ast.Declaration.TypeSynonym
import           HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs     as GHC
import           HIndent.Pretty
import           HIndent.Pretty.NodeComments

data Declaration
  = DataFamily DataFamily
  | TypeFamily TypeFamily
  | TypeSynonym TypeSynonym
  | TyClDecl (GHC.TyClDecl GHC.GhcPs)
  | ClassInstance ClassInstance
  | InstDecl (GHC.InstDecl GHC.GhcPs)
  | DerivDecl (GHC.DerivDecl GHC.GhcPs)
  | ValDecl (GHC.HsBind GHC.GhcPs)
  | SigDecl (GHC.Sig GHC.GhcPs)
  | KindSigDecl (GHC.StandaloneKindSig GHC.GhcPs)
  | DefDecl (GHC.DefaultDecl GHC.GhcPs)
  | ForDecl (GHC.ForeignDecl GHC.GhcPs)
  | WarningDecl (GHC.WarnDecls GHC.GhcPs)
  | AnnDecl (GHC.AnnDecl GHC.GhcPs)
  | RuleDecl (GHC.RuleDecls GHC.GhcPs)
  | SpliceDecl (GHC.SpliceDecl GHC.GhcPs)
  | RoleAnnotDecl (GHC.RoleAnnotDecl GHC.GhcPs)

instance CommentExtraction Declaration where
  nodeComments DataFamily {}    = NodeComments [] [] []
  nodeComments TypeFamily {}    = NodeComments [] [] []
  nodeComments TypeSynonym {}   = NodeComments [] [] []
  nodeComments TyClDecl {}      = NodeComments [] [] []
  nodeComments ClassInstance {} = NodeComments [] [] []
  nodeComments InstDecl {}      = NodeComments [] [] []
  nodeComments DerivDecl {}     = NodeComments [] [] []
  nodeComments ValDecl {}       = NodeComments [] [] []
  nodeComments SigDecl {}       = NodeComments [] [] []
  nodeComments KindSigDecl {}   = NodeComments [] [] []
  nodeComments DefDecl {}       = NodeComments [] [] []
  nodeComments ForDecl {}       = NodeComments [] [] []
  nodeComments WarningDecl {}   = NodeComments [] [] []
  nodeComments AnnDecl {}       = NodeComments [] [] []
  nodeComments RuleDecl {}      = NodeComments [] [] []
  nodeComments SpliceDecl {}    = NodeComments [] [] []
  nodeComments RoleAnnotDecl {} = NodeComments [] [] []

instance Pretty Declaration where
  pretty' (DataFamily x)    = pretty x
  pretty' (TypeFamily x)    = pretty x
  pretty' (TypeSynonym x)   = pretty x
  pretty' (TyClDecl x)      = pretty x
  pretty' (ClassInstance x) = pretty x
  pretty' (InstDecl x)      = pretty x
  pretty' (DerivDecl x)     = pretty x
  pretty' (ValDecl x)       = pretty x
  pretty' (SigDecl x)       = pretty x
  pretty' (KindSigDecl x)   = pretty x
  pretty' (DefDecl x)       = pretty x
  pretty' (ForDecl x)       = pretty x
  pretty' (WarningDecl x)   = pretty x
  pretty' (AnnDecl x)       = pretty x
  pretty' (RuleDecl x)      = pretty x
  pretty' (SpliceDecl x)    = pretty x
  pretty' (RoleAnnotDecl x) = pretty x

mkDeclaration :: GHC.HsDecl GHC.GhcPs -> Declaration
mkDeclaration (GHC.TyClD _ (GHC.FamDecl _ x))
  | GHC.DataFamily <- GHC.fdInfo x = DataFamily $ mkDataFamily x
  | otherwise = TypeFamily $ mkTypeFamily x
mkDeclaration (GHC.TyClD _ x@GHC.SynDecl {}) = TypeSynonym $ mkTypeSynonym x
mkDeclaration (GHC.TyClD _ x) = TyClDecl x
mkDeclaration (GHC.InstD _ x)
  | Just inst <- mkClassInstance x = ClassInstance inst
  | otherwise = InstDecl x
mkDeclaration (GHC.DerivD _ x) = DerivDecl x
mkDeclaration (GHC.ValD _ x) = ValDecl x
mkDeclaration (GHC.SigD _ x) = SigDecl x
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
isSignature SigDecl {} = True
isSignature _          = False
