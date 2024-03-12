{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards   #-}

module HIndent.Ast.WithComments
  ( WithComments
  , fromGenLocated
  , fromEpAnn
  , getNode
  ) where

import           Control.Monad
import           Control.Monad.RWS
import qualified GHC.Hs                      as GHC
import qualified GHC.Types.SrcLoc            as GHC
import           HIndent.Ast.NodeComments    (NodeComments (..))
import qualified HIndent.Ast.NodeComments    as NodeComments
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments
import           HIndent.Printer

data WithComments a = WithComments
  { comments :: NodeComments
  , node     :: a
  } deriving (Foldable, Traversable)

instance Functor WithComments where
  fmap f WithComments {..} = WithComments comments (f node)

instance CommentExtraction (WithComments a) where
  nodeComments WithComments {..} = comments

fromGenLocated :: (CommentExtraction l) => GHC.GenLocated l a -> WithComments a
fromGenLocated (GHC.L l a) = WithComments (nodeComments l) a

fromEpAnn :: GHC.EpAnn a -> b -> WithComments b
fromEpAnn ann = WithComments (NodeComments.fromEpAnn ann)

getNode :: WithComments a -> a
getNode = node
