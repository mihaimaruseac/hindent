module HIndent.Ast.Declaration.Family
  ( FamilyDeclaration
  , mkFamilyDeclaration
  ) where

import           HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import           HIndent.Pretty
import           HIndent.Pretty.NodeComments

newtype FamilyDeclaration =
  FamilyDeclaration (GHC.FamilyDecl GHC.GhcPs)

instance CommentExtraction FamilyDeclaration where
  nodeComments FamilyDeclaration {} = NodeComments [] [] []

instance Pretty FamilyDeclaration where
  pretty' (FamilyDeclaration x) = pretty x

mkFamilyDeclaration :: GHC.FamilyDecl GHC.GhcPs -> FamilyDeclaration
mkFamilyDeclaration = FamilyDeclaration
