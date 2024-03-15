{-# LANGUAGE CPP #-}

module HIndent.Ast.Declaration.Data
  ( DataDeclaration(..)
  , mkDataDeclaration
  ) where

import           HIndent.Ast.Declaration.Data.Body
import           HIndent.Ast.Declaration.Data.Header
import           HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs  as GHC
import           HIndent.Pretty.NodeComments

data DataDeclaration = DataDeclaration
  { header :: Header
  , body   :: DataBody
  }

instance CommentExtraction DataDeclaration where
  nodeComments DataDeclaration {} = NodeComments [] [] []

mkDataDeclaration :: GHC.TyClDecl GHC.GhcPs -> Maybe DataDeclaration
mkDataDeclaration decl = DataDeclaration <$> mkHeader decl <*> mkDataBody decl
