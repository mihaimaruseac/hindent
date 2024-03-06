module HIndent.Ast.Declaration
  ( Declaration(..)
  , mkDeclaration
  , isSignature
  ) where

import           HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import           HIndent.Pretty
import           HIndent.Pretty.NodeComments

data Declaration
  = TyClDecl (GHC.TyClDecl GHC.GhcPs)
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
  | DocDecl GHC.DocDecl
  | RoleAnnotDecl (GHC.RoleAnnotDecl GHC.GhcPs)

instance CommentExtraction Declaration where
  nodeComments TyClDecl {}      = NodeComments [] [] []
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
  nodeComments DocDecl {}       = NodeComments [] [] []
  nodeComments RoleAnnotDecl {} = NodeComments [] [] []

instance Pretty Declaration where
  pretty' (TyClDecl x)      = pretty x
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
  pretty' (DocDecl _)       = undefined
  pretty' (RoleAnnotDecl x) = pretty x

mkDeclaration :: GHC.HsDecl GHC.GhcPs -> Declaration
mkDeclaration (GHC.TyClD _ x)      = TyClDecl x
mkDeclaration (GHC.InstD _ x)      = InstDecl x
mkDeclaration (GHC.DerivD _ x)     = DerivDecl x
mkDeclaration (GHC.ValD _ x)       = ValDecl x
mkDeclaration (GHC.SigD _ x)       = SigDecl x
mkDeclaration (GHC.KindSigD _ x)   = KindSigDecl x
mkDeclaration (GHC.DefD _ x)       = DefDecl x
mkDeclaration (GHC.ForD _ x)       = ForDecl x
mkDeclaration (GHC.WarningD _ x)   = WarningDecl x
mkDeclaration (GHC.AnnD _ x)       = AnnDecl x
mkDeclaration (GHC.RuleD _ x)      = RuleDecl x
mkDeclaration (GHC.SpliceD _ x)    = SpliceDecl x
mkDeclaration (GHC.DocD _ x)       = DocDecl x
mkDeclaration (GHC.RoleAnnotD _ x) = RoleAnnotDecl x

isSignature :: Declaration -> Bool
isSignature SigDecl {} = True
isSignature _          = False
