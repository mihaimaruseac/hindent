{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Build 'CommentGroup' values from types that can appear in the
-- location slot of 'GHC.GenLocated'.
module HIndent.Ast.IsGenLocatedLocation
  ( CommentGroup(..)
  , IsGenLocatedLocation(..)
  , fromNodeComments
  , mkCommentGroupFromEpAnn
  , mkCommentGroupFromEpaLocation
  ) where

import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.Comment (Comment, mkComment)
import qualified HIndent.Ast.NodeComments as NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pragma

data CommentGroup = CommentGroup
  { commentsBefore :: [Comment]
  , commentsOnSameLine :: [Comment]
  , commentsAfter :: [Comment]
  } deriving (Eq)

class IsGenLocatedLocation a where
  mkCommentGroupFromGenLocatedLocation :: a -> CommentGroup

instance Semigroup CommentGroup where
  CommentGroup beforeX sameLineX afterX <> CommentGroup beforeY sameLineY afterY =
    CommentGroup
      { commentsBefore = beforeX <> beforeY
      , commentsOnSameLine = sameLineX <> sameLineY
      , commentsAfter = afterX <> afterY
      }

instance Monoid CommentGroup where
  mempty =
    CommentGroup
      {commentsBefore = [], commentsOnSameLine = [], commentsAfter = []}

instance IsGenLocatedLocation GHC.EpAnnComments where
  mkCommentGroupFromGenLocatedLocation = mkCommentGroupFromEpAnnComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
instance IsGenLocatedLocation (GHC.EpAnn ann) where
  mkCommentGroupFromGenLocatedLocation = mkCommentGroupFromEpAnn
#endif
instance IsGenLocatedLocation GHC.SrcSpan where
  mkCommentGroupFromGenLocatedLocation _ = mempty

instance IsGenLocatedLocation GHC.NoEpAnns where
  mkCommentGroupFromGenLocatedLocation _ = mempty

fromNodeComments :: NodeComments.NodeComments -> CommentGroup
fromNodeComments (NodeComments.NodeComments before sameLine after) =
  CommentGroup
    { commentsBefore = mkComment <$> before
    , commentsOnSameLine = mkComment <$> sameLine
    , commentsAfter = mkComment <$> after
    }

mkCommentGroupFromEpAnn :: GHC.EpAnn a -> CommentGroup
mkCommentGroupFromEpAnn =
  mkCommentGroupFromEpAnn' . filterOutEofAndPragmasFromAnn
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkCommentGroupFromEpaLocation :: GHC.EpaLocation -> CommentGroup
mkCommentGroupFromEpaLocation GHC.EpaSpan {} = mempty
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkCommentGroupFromEpaLocation (GHC.EpaDelta _ _ trailing) =
  foldMap mkCommentGroupFromTrailingComment trailing
#else
mkCommentGroupFromEpaLocation (GHC.EpaDelta _ trailing) =
  foldMap mkCommentGroupFromTrailingComment trailing
#endif
#else
mkCommentGroupFromEpaLocation :: GHC.Anchor -> CommentGroup
mkCommentGroupFromEpaLocation _ = mempty
#endif
mkCommentGroupFromEpAnnComments :: GHC.EpAnnComments -> CommentGroup
mkCommentGroupFromEpAnnComments comments = CommentGroup {..}
  where
    commentsBefore = mkComment <$> GHC.priorComments filteredComments
    commentsOnSameLine = []
    commentsAfter = mkComment <$> GHC.getFollowingComments filteredComments
    filteredComments = filterOutEofAndPragmasFromComments comments

filterOutEofAndPragmasFromAnn :: GHC.EpAnn ann -> GHC.EpAnn ann
filterOutEofAndPragmasFromAnn GHC.EpAnn {..} =
  GHC.EpAnn {comments = filterOutEofAndPragmasFromComments comments, ..}
#if !MIN_VERSION_ghc_lib_parser(9, 10, 1)
filterOutEofAndPragmasFromAnn GHC.EpAnnNotUsed = GHC.EpAnnNotUsed
#endif
mkCommentGroupFromEpAnn' :: GHC.EpAnn a -> CommentGroup
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkCommentGroupFromEpAnn' GHC.EpAnn {..} =
  mkCommentGroupFromEntry entry <> CommentGroup {..}
  where
    commentsBefore = mkComment <$> GHC.priorComments comments
    commentsOnSameLine =
      fmap mkComment
        $ filter isCommentOnSameLine
        $ GHC.getFollowingComments comments
    commentsAfter =
      fmap mkComment
        $ filter (not . isCommentOnSameLine)
        $ GHC.getFollowingComments comments
    isCommentOnSameLine (GHC.L commentLoc _) =
      GHC.srcSpanEndLine (epaLocationToRealSrcSpan entry)
        == GHC.srcSpanStartLine (epaLocationToRealSrcSpan commentLoc)
#else
mkCommentGroupFromEpAnn' GHC.EpAnn {..} =
  mkCommentGroupFromEntry entry <> CommentGroup {..}
  where
    commentsBefore = mkComment <$> GHC.priorComments comments
    commentsOnSameLine =
      fmap mkComment
        $ filter isCommentOnSameLine
        $ GHC.getFollowingComments comments
    commentsAfter =
      fmap mkComment
        $ filter (not . isCommentOnSameLine)
        $ GHC.getFollowingComments comments
    isCommentOnSameLine (GHC.L commentLoc _) =
      GHC.srcSpanEndLine (epaLocationToRealSrcSpan entry)
        == GHC.srcSpanStartLine (epaLocationToRealSrcSpan commentLoc)
mkCommentGroupFromEpAnn' GHC.EpAnnNotUsed = mempty
#endif
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
#if !MIN_VERSION_ghc_lib_parser(9, 10, 1)
isNeitherEofNorPragmaComment (GHC.L _ (GHC.EpaComment GHC.EpaEofComment _)) =
  False
#endif
isNeitherEofNorPragmaComment (GHC.L _ (GHC.EpaComment token _)) =
  not $ isPragma token
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkCommentGroupFromTrailingComment :: GHC.LEpaComment -> CommentGroup
mkCommentGroupFromTrailingComment comment =
  mempty {commentsAfter = [mkComment comment]}
#endif

#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkCommentGroupFromEntry :: GHC.EpaLocation -> CommentGroup
#else
mkCommentGroupFromEntry :: GHC.Anchor -> CommentGroup
#endif
mkCommentGroupFromEntry = mkCommentGroupFromEpaLocation
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
epaLocationToRealSrcSpan :: GHC.EpaLocation' a -> GHC.RealSrcSpan
epaLocationToRealSrcSpan = GHC.epaLocationRealSrcSpan
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
epaLocationToRealSrcSpan :: GHC.EpaLocation' a -> GHC.RealSrcSpan
epaLocationToRealSrcSpan = GHC.anchor
#else
epaLocationToRealSrcSpan :: GHC.Anchor -> GHC.RealSrcSpan
epaLocationToRealSrcSpan = GHC.anchor
#endif
