module HIndent.Ast.WithComments
  ( WithComments
  , addComments
  , fromGenLocated
  , getComments
  , getNode
  , mkWithComments
  ) where

import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.NodeComments (NodeComments)
import HIndent.Pretty.NodeComments (CommentExtraction)

data WithComments a

addComments :: NodeComments -> WithComments a -> WithComments a
fromGenLocated :: CommentExtraction l => GHC.GenLocated l a -> WithComments a
getComments :: WithComments a -> NodeComments
getNode :: WithComments a -> a
mkWithComments :: a -> WithComments a
