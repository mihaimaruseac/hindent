module HIndent.Ast.Declaration.Class
  ( ClassDeclaration(..)
  , mkClassDeclaration
  ) where

import           HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import           HIndent.Pretty.NodeComments

newtype ClassDeclaration = ClassDeclaration
  { decl :: GHC.TyClDecl GHC.GhcPs
  }

instance CommentExtraction ClassDeclaration where
  nodeComments (ClassDeclaration _) = NodeComments [] [] []

mkClassDeclaration :: GHC.TyClDecl GHC.GhcPs -> ClassDeclaration
mkClassDeclaration = ClassDeclaration
