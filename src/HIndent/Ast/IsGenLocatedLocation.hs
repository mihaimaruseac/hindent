{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Build 'CommentGroup' values from types that can appear in the
-- location slot of 'GHC.GenLocated'.
--
-- This module exists so 'mkWithCommentsFromGenLocated' can stay
-- generic over GHC-side location and annotation types. It is fine for
-- an instance to return empty comments, such as for 'GHC.SrcSpan' and
-- 'GHC.NoEpAnns'.
--
-- Do not implement this class for GHC AST node bodies such as
-- expressions or declarations. Smart constructors handle those
-- directly. Do not implement it for Hindent AST types either; Hindent
-- AST values keep comments via 'WithComments'.
module HIndent.Ast.IsGenLocatedLocation
  ( CommentGroup(..)
  , IsGenLocatedLocation(..)
  , mkCommentGroupFromEpAnn
  , mkCommentGroupFromEpaLocation
  ) where

import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.Comment (Comment, mkComment)
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
  x <> y =
    CommentGroup
      { commentsBefore = commentsBefore x <> commentsBefore y
      , commentsOnSameLine = commentsOnSameLine x <> commentsOnSameLine y
      , commentsAfter = commentsAfter x <> commentsAfter y
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

mkCommentGroupFromEpAnn :: GHC.EpAnn a -> CommentGroup
mkCommentGroupFromEpAnn =
  mkCommentGroupFromEpAnn' . filterOutEofAndPragmasFromAnn

mkCommentGroupFromEpAnnComments :: GHC.EpAnnComments -> CommentGroup
mkCommentGroupFromEpAnnComments comments = CommentGroup {..}
  where
    commentsBefore = mkComment <$> GHC.priorComments filteredComments
    commentsOnSameLine = []
    commentsAfter = mkComment <$> GHC.getFollowingComments filteredComments
    filteredComments = filterOutEofAndPragmasFromComments comments
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
#endif
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

mkCommentGroupFromEntry :: GHC.EpaLocation -> CommentGroup
mkCommentGroupFromEntry = mkCommentGroupFromEpaLocation
#else
mkCommentGroupFromEpaLocation :: GHC.Anchor -> CommentGroup
mkCommentGroupFromEpaLocation _ = mempty

mkCommentGroupFromEntry :: GHC.Anchor -> CommentGroup
mkCommentGroupFromEntry = mkCommentGroupFromEpaLocation
#endif

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
