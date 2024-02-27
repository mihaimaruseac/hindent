module HIndent.Ast.WithComments
  ( WithComments
  , mkWithComments
  ) where

import           HIndent.Pretty
import           HIndent.Pretty.NodeComments
import           HIndent.Pretty.Types

data WithComments a = WithComments
  { comments :: NodeComments
  , _node    :: a
  }

instance CommentExtraction (WithComments a) where
  nodeComments = comments

instance (Pretty a) => Pretty (WithComments a) where
  pretty' (WithComments _ x) = pretty' x

mkWithComments :: a -> WithComments a
mkWithComments = WithComments (NodeComments [] [] [])
