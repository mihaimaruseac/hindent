{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Class
  ( ClassDeclaration(..)
  , mkClassDeclaration
  ) where

import           HIndent.Ast.Context
import           HIndent.Ast.NodeComments
import           HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import           HIndent.Pretty.NodeComments

data ClassDeclaration = ClassDeclaration
  { context :: Maybe (WithComments Context)
  , decl    :: GHC.TyClDecl GHC.GhcPs
  }

instance CommentExtraction ClassDeclaration where
  nodeComments ClassDeclaration {} = NodeComments [] [] []

mkClassDeclaration :: GHC.TyClDecl GHC.GhcPs -> Maybe ClassDeclaration
mkClassDeclaration x@GHC.ClassDecl {..} = Just ClassDeclaration {..}
  where
    context = fmap (fmap mkContext . fromGenLocated) tcdCtxt
    decl = x
mkClassDeclaration _ = Nothing
