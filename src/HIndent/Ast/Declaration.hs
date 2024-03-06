module HIndent.Ast.Declaration
  ( Declaration(..)
  , mkDeclaration
  , isSignature
  ) where

import           HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import           HIndent.Pretty
import           HIndent.Pretty.NodeComments

newtype Declaration =
  Declaration (GHC.HsDecl GHC.GhcPs)

instance CommentExtraction Declaration where
  nodeComments (Declaration _) = NodeComments [] [] []

instance Pretty Declaration where
  pretty' (Declaration decl) = pretty decl

mkDeclaration :: GHC.HsDecl GHC.GhcPs -> Declaration
mkDeclaration x@GHC.TyClD {}      = Declaration x
mkDeclaration x@GHC.InstD {}      = Declaration x
mkDeclaration x@GHC.DerivD {}     = Declaration x
mkDeclaration x@GHC.ValD {}       = Declaration x
mkDeclaration x@GHC.SigD {}       = Declaration x
mkDeclaration x@GHC.KindSigD {}   = Declaration x
mkDeclaration x@GHC.DefD {}       = Declaration x
mkDeclaration x@GHC.ForD {}       = Declaration x
mkDeclaration x@GHC.WarningD {}   = Declaration x
mkDeclaration x@GHC.AnnD {}       = Declaration x
mkDeclaration x@GHC.RuleD {}      = Declaration x
mkDeclaration x@GHC.SpliceD {}    = Declaration x
mkDeclaration x@GHC.DocD {}       = Declaration x
mkDeclaration x@GHC.RoleAnnotD {} = Declaration x

isSignature :: Declaration -> Bool
isSignature (Declaration (GHC.SigD _ _)) = True
isSignature _                            = False
