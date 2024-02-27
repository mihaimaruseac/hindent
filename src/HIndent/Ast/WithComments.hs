{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.WithComments
  ( WithComments
  , mkWithComments
  ) where

import HIndent.Pretty
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Types

data WithComments a = WithComments
  { comments :: NodeComments
  , node :: a
  }

instance CommentExtraction (WithComments a) where
  nodeComments = comments

instance (Pretty a) => Pretty (WithComments a) where
  pretty' WithComments {..} = pretty' node

mkWithComments :: a -> WithComments a
mkWithComments = WithComments (NodeComments [] [] [])
