{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Data
  ( DataDeclaration
  , mkDataDeclaration
  ) where

import HIndent.Ast.Declaration.Data.Body
import HIndent.Ast.Declaration.Data.Header
import HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.NodeComments

data DataDeclaration = DataDeclaration
  { header :: Header
  , body :: DataBody
  }

instance CommentExtraction DataDeclaration where
  nodeComments DataDeclaration {} = NodeComments [] [] []

instance Pretty DataDeclaration where
  pretty' DataDeclaration {..} = pretty header >> pretty body

mkDataDeclaration :: GHC.TyClDecl GHC.GhcPs -> Maybe DataDeclaration
mkDataDeclaration decl@GHC.DataDecl {..} =
  DataDeclaration <$> mkHeader decl <*> pure (mkDataBody tcdDataDefn)
mkDataDeclaration _ = Nothing
