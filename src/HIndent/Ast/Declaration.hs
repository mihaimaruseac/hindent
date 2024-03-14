module HIndent.Ast.Declaration
  ( Declaration(..)
  , mkDeclaration
  , isSignature
  ) where

import Control.Applicative
import Data.Maybe
import qualified HIndent.Ast.Declaration.Class
import HIndent.Ast.Declaration.Data
import qualified HIndent.Ast.Declaration.Family.Data
import qualified HIndent.Ast.Declaration.Family.Type
import qualified HIndent.Ast.Declaration.Instance.Class
import qualified HIndent.Ast.Declaration.TypeSynonym
import HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty.NodeComments

data Declaration
  = DataFamily HIndent.Ast.Declaration.Family.Data.DataFamily
  | TypeFamily HIndent.Ast.Declaration.Family.Type.TypeFamily
  | DataDeclaration DataDeclaration
  | ClassDeclaration HIndent.Ast.Declaration.Class.ClassDeclaration
  | TypeSynonym HIndent.Ast.Declaration.TypeSynonym.TypeSynonym
  | TyClDecl (GHC.TyClDecl GHC.GhcPs)
  | ClassInstance HIndent.Ast.Declaration.Instance.Class.ClassInstance
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
  nodeComments DataFamily {} = NodeComments [] [] []
  nodeComments TypeFamily {} = NodeComments [] [] []
  nodeComments DataDeclaration {} = NodeComments [] [] []
  nodeComments ClassDeclaration {} = NodeComments [] [] []
  nodeComments TypeSynonym {} = NodeComments [] [] []
  nodeComments TyClDecl {} = NodeComments [] [] []
  nodeComments ClassInstance {} = NodeComments [] [] []
  nodeComments InstDecl {} = NodeComments [] [] []
  nodeComments DerivDecl {} = NodeComments [] [] []
  nodeComments ValDecl {} = NodeComments [] [] []
  nodeComments SigDecl {} = NodeComments [] [] []
  nodeComments KindSigDecl {} = NodeComments [] [] []
  nodeComments DefDecl {} = NodeComments [] [] []
  nodeComments ForDecl {} = NodeComments [] [] []
  nodeComments WarningDecl {} = NodeComments [] [] []
  nodeComments AnnDecl {} = NodeComments [] [] []
  nodeComments RuleDecl {} = NodeComments [] [] []
  nodeComments SpliceDecl {} = NodeComments [] [] []
  nodeComments RoleAnnotDecl {} = NodeComments [] [] []

mkDeclaration :: GHC.HsDecl GHC.GhcPs -> Declaration
mkDeclaration (GHC.TyClD _ (GHC.FamDecl _ x)) =
  fromMaybe (error "Unreachable.")
    $ DataFamily <$> HIndent.Ast.Declaration.Family.Data.mkDataFamily x
        <|> TypeFamily <$> HIndent.Ast.Declaration.Family.Type.mkTypeFamily x
mkDeclaration (GHC.TyClD _ x@GHC.SynDecl {}) =
  TypeSynonym $ HIndent.Ast.Declaration.TypeSynonym.mkTypeSynonym x
mkDeclaration (GHC.TyClD _ x@(GHC.DataDecl {}))
  | Just decl <- mkDataDeclaration x = DataDeclaration decl
mkDeclaration (GHC.TyClD _ x) = TyClDecl x
mkDeclaration (GHC.InstD _ x)
  | Just inst <- HIndent.Ast.Declaration.Instance.Class.mkClassInstance x =
    ClassInstance inst
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
isSignature _ = False
