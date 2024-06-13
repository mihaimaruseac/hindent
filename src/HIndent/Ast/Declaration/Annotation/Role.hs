{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Annotation.Role
  ( RoleAnnotation
  , mkRoleAnnotation
  ) where

import HIndent.Ast.NodeComments
import HIndent.Ast.Role
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data RoleAnnotation = RoleAnnotation
  { name :: GHC.LIdP GHC.GhcPs
  , roles :: [WithComments (Maybe Role)]
  }

instance CommentExtraction RoleAnnotation where
  nodeComments RoleAnnotation {} = NodeComments [] [] []

instance Pretty RoleAnnotation where
  pretty' RoleAnnotation {..} =
    spaced
      $ [string "type role", pretty name]
          ++ fmap (`prettyWith` maybe (string "_") pretty) roles

mkRoleAnnotation :: GHC.RoleAnnotDecl GHC.GhcPs -> RoleAnnotation
mkRoleAnnotation (GHC.RoleAnnotDecl _ name rs) = RoleAnnotation {..}
  where
    roles = fmap (fmap (fmap mkRole) . fromGenLocated) rs
