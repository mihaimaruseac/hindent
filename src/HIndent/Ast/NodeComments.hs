module HIndent.Ast.NodeComments
  ( NodeComments(..)
  ) where

import qualified GHC.Hs as GHC

-- | Comments belonging to an AST node.
data NodeComments = NodeComments
  { commentsBefore :: [GHC.LEpaComment]
  , commentsOnSameLine :: [GHC.LEpaComment]
  , commentsAfter :: [GHC.LEpaComment]
  }

instance Semigroup NodeComments where
  x <> y =
    NodeComments
      { commentsBefore = commentsBefore x <> commentsBefore y
      , commentsOnSameLine = commentsOnSameLine x <> commentsOnSameLine y
      , commentsAfter = commentsAfter x <> commentsAfter y
      }
