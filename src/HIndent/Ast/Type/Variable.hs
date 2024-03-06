module HIndent.Ast.Type.Variable
  ( TypeVariable
  , mkTypeVariable
  ) where

import qualified GHC.Hs as GHC
import HIndent.Ast.NodeComments
import HIndent.Pretty
import HIndent.Pretty.NodeComments

newtype TypeVariable a =
  TypeVariable (GHC.HsTyVarBndr a GHC.GhcPs)

instance CommentExtraction (TypeVariable a) where
  nodeComments (TypeVariable _) = NodeComments [] [] []

instance Pretty (TypeVariable a) where
  pretty' (TypeVariable x) = pretty x

mkTypeVariable :: GHC.HsTyVarBndr a GHC.GhcPs -> TypeVariable a
mkTypeVariable = TypeVariable
