module HIndent.Ast.Declaration.TypeSynonym
  ( TypeSynonym
  , mkTypeSynonym
  ) where

import           HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import           HIndent.Pretty
import           HIndent.Pretty.NodeComments

newtype TypeSynonym =
  TypeSynonym (GHC.TyClDecl GHC.GhcPs)

instance CommentExtraction TypeSynonym where
  nodeComments (TypeSynonym _) = NodeComments [] [] []

instance Pretty TypeSynonym where
  pretty' (TypeSynonym x) = pretty x

mkTypeSynonym :: GHC.TyClDecl GHC.GhcPs -> TypeSynonym
mkTypeSynonym = TypeSynonym
