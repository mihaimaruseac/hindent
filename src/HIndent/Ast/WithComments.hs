{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.WithComments
  ( WithComments
  , fromEpAnn
  ) where

import qualified GHC.Hs as GHC
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.NodeComments
import HIndent.Pretty
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Pragma

data WithComments a = WithComments
  { comments :: NodeComments
  , node :: a
  }

instance CommentExtraction (WithComments a) where
  nodeComments WithComments {..} = comments

instance (Pretty a) => Pretty (WithComments a) where
  pretty' WithComments {..} = pretty' node

fromEpAnn :: GHC.EpAnn a -> b -> WithComments b
fromEpAnn ann = WithComments (epaComments $ filterOutEofAndPragmasFromAnn ann)

epaComments :: GHC.EpAnn a -> NodeComments
epaComments GHC.EpAnn {..} = NodeComments {..}
  where
    commentsBefore = GHC.priorComments comments
    commentsOnSameLine =
      filter isCommentOnSameLine $ GHC.getFollowingComments comments
    commentsAfter =
      filter (not . isCommentOnSameLine) $ GHC.getFollowingComments comments
    isCommentOnSameLine (GHC.L comAnn _) =
      GHC.srcSpanEndLine (GHC.anchor entry)
        == GHC.srcSpanStartLine (GHC.anchor comAnn)
epaComments GHC.EpAnnNotUsed = NodeComments [] [] []

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
