{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.WithComments
  ( WithComments
  , fromEpAnn
  ) where

import qualified GHC.Hs as GHC
import HIndent.Ast.NodeComments (NodeComments(..))
import qualified HIndent.Ast.NodeComments as NodeComments
import HIndent.Pretty
import HIndent.Pretty.NodeComments

data WithComments a = WithComments
  { comments :: NodeComments
  , node :: a
  }

instance CommentExtraction (WithComments a) where
  nodeComments WithComments {..} = comments

instance (Pretty a) => Pretty (WithComments a) where
  pretty' WithComments {..} = pretty' node

fromEpAnn :: GHC.EpAnn a -> b -> WithComments b
fromEpAnn ann = WithComments (NodeComments.fromEpAnn ann)
