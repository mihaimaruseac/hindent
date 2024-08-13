{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.NodeComments
  ( NodeComments(..)
  , fromEpAnn
  ) where

import qualified GHC.Hs as GHC
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Pragma

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

instance Monoid NodeComments where
  mempty =
    NodeComments
      {commentsBefore = [], commentsOnSameLine = [], commentsAfter = []}

fromEpAnn :: GHC.EpAnn a -> NodeComments
fromEpAnn = fromEpAnn' . filterOutEofAndPragmasFromAnn

fromEpAnn' :: GHC.EpAnn a -> NodeComments
fromEpAnn' GHC.EpAnn {..} = NodeComments {..}
  where
    commentsBefore = GHC.priorComments comments
    commentsOnSameLine =
      filter isCommentOnSameLine $ GHC.getFollowingComments comments
    commentsAfter =
      filter (not . isCommentOnSameLine) $ GHC.getFollowingComments comments
    isCommentOnSameLine (GHC.L comAnn _) =
      GHC.srcSpanEndLine (GHC.anchor entry)
        == GHC.srcSpanStartLine (GHC.anchor comAnn)
fromEpAnn' GHC.EpAnnNotUsed = NodeComments [] [] []

filterOutEofAndPragmasFromAnn :: GHC.EpAnn ann -> GHC.EpAnn ann
filterOutEofAndPragmasFromAnn GHC.EpAnn {..} =
  GHC.EpAnn {comments = filterOutEofAndPragmasFromComments comments, ..}
filterOutEofAndPragmasFromAnn GHC.EpAnnNotUsed = GHC.EpAnnNotUsed

filterOutEofAndPragmasFromComments :: GHC.EpAnnComments -> GHC.EpAnnComments
filterOutEofAndPragmasFromComments comments =
  GHC.EpaCommentsBalanced
    { priorComments = filterOutEofAndPragmas $ GHC.priorComments comments
    , followingComments =
        filterOutEofAndPragmas $ GHC.getFollowingComments comments
    }

filterOutEofAndPragmas ::
     [GHC.GenLocated l GHC.EpaComment] -> [GHC.GenLocated l GHC.EpaComment]
filterOutEofAndPragmas = filter isNeitherEofNorPragmaComment

isNeitherEofNorPragmaComment :: GHC.GenLocated l GHC.EpaComment -> Bool
isNeitherEofNorPragmaComment (GHC.L _ (GHC.EpaComment GHC.EpaEofComment _)) =
  False
isNeitherEofNorPragmaComment (GHC.L _ (GHC.EpaComment tok _)) =
  not $ isPragma tok
