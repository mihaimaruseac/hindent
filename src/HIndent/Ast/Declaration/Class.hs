module HIndent.Ast.Declaration.Class
  ( ClassDeclaration(..)
  ) where

import HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty.NodeComments

newtype ClassDeclaration =
  ClassDeclaration (GHC.TyClDecl GHC.GhcPs)

instance CommentExtraction ClassDeclaration where
  nodeComments (ClassDeclaration _) = NodeComments [] [] []
