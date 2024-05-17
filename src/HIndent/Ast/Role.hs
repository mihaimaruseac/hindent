module HIndent.Ast.Role
  ( Role
  , mkRole
  ) where

import qualified GHC.Core.TyCon as GHC
import HIndent.Ast.NodeComments
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data Role
  = Nominal
  | Representational
  | Phantom

instance CommentExtraction Role where
  nodeComments Nominal = NodeComments [] [] []
  nodeComments Representational = NodeComments [] [] []
  nodeComments Phantom = NodeComments [] [] []

instance Pretty Role where
  pretty' Nominal = string "nominal"
  pretty' Representational = string "representational"
  pretty' Phantom = string "phantom"

mkRole :: GHC.Role -> Role
mkRole GHC.Nominal = Nominal
mkRole GHC.Representational = Representational
mkRole GHC.Phantom = Phantom
