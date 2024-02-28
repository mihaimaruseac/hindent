{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.WithComments
  ( WithComments
  , fromGenLocated
  , fromEpAnn
  ) where

import qualified GHC.Hs as GHC
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.NodeComments (NodeComments(..))
import qualified HIndent.Ast.NodeComments as NodeComments
import HIndent.Pretty
import HIndent.Pretty.NodeComments

data WithComments a = WithComments
  { comments :: NodeComments
  , node :: a
  }

instance Functor WithComments where
  fmap f WithComments {..} = WithComments comments (f node)

instance CommentExtraction (WithComments a) where
  nodeComments WithComments {..} = comments

instance (Pretty a) => Pretty (WithComments a) where
  pretty' WithComments {..} = pretty' node

fromGenLocated :: (CommentExtraction l) => GHC.GenLocated l a -> WithComments a
fromGenLocated (GHC.L l a) = WithComments (nodeComments l) a

fromEpAnn :: GHC.EpAnn a -> b -> WithComments b
fromEpAnn ann = WithComments (NodeComments.fromEpAnn ann)
