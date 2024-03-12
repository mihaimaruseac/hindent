{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.TypeSynonym
  ( TypeSynonym
  , mkTypeSynonym
  ) where

import           HIndent.Ast.Declaration.TypeSynonym.Lhs
import           HIndent.Ast.NodeComments
import           HIndent.Ast.Type
import           HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs      as GHC
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments

data TypeSynonym = TypeSynonym
  { lhs :: TypeSynonymLhs
  , rhs :: WithComments Type
  }

instance CommentExtraction TypeSynonym where
  nodeComments TypeSynonym {} = NodeComments [] [] []

mkTypeSynonym :: GHC.TyClDecl GHC.GhcPs -> TypeSynonym
mkTypeSynonym synonym@GHC.SynDecl {..} = TypeSynonym {..}
  where
    lhs = mkTypeSynonymLhs synonym
    rhs = mkType <$> fromGenLocated tcdRhs
mkTypeSynonym _ = error "Not a type synonym."
