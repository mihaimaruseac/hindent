{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module HIndent.Ast.WithComments
  ( WithComments
  , prettyWith
  , fromGenLocated
  , fromEpAnn
  , mkWithComments
  , getNode
  , getComments
  , addComments
  , flattenComments
  ) where

import Control.Monad
import Control.Monad.RWS
import qualified GHC.Hs as GHC
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.NodeComments (NodeComments(..))
import qualified HIndent.Ast.NodeComments as NodeComments
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Printer

data WithComments a = WithComments
  { comments :: NodeComments
  , node :: a
  } deriving (Foldable, Traversable, Eq)

instance Functor WithComments where
  fmap f WithComments {..} = WithComments comments (f node)

instance CommentExtraction (WithComments a) where
  nodeComments WithComments {..} = comments

instance (Pretty a) => Pretty (WithComments a) where
  pretty' WithComments {..} = pretty' node

-- | Prints comments included in the location information and then the
-- AST node body.
prettyWith :: WithComments a -> (a -> Printer ()) -> Printer ()
prettyWith WithComments {..} f = do
  printCommentsBefore comments
  f node
  printCommentOnSameLine comments
  printCommentsAfter comments

-- | Prints comments that are before the given AST node.
printCommentsBefore :: NodeComments -> Printer ()
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
printCommentsBefore p =
  forM_ (commentsBefore p) $ \(GHC.L loc c) -> do
    let col =
          fromIntegral
            $ GHC.srcSpanStartCol (GHC.epaLocationRealSrcSpan loc) - 1
    indentedWithFixedLevel col $ pretty c
    newline
#else
printCommentsBefore p =
  forM_ (commentsBefore p) $ \(GHC.L loc c) -> do
    let col = fromIntegral $ GHC.srcSpanStartCol (GHC.anchor loc) - 1
    indentedWithFixedLevel col $ pretty c
    newline
#endif
-- | Prints comments that are on the same line as the given AST node.
printCommentOnSameLine :: NodeComments -> Printer ()
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
printCommentOnSameLine (commentsOnSameLine -> (c:cs)) = do
  col <- gets psColumn
  if col == 0
    then indentedWithFixedLevel
           (fromIntegral
              $ GHC.srcSpanStartCol
              $ GHC.epaLocationRealSrcSpan
              $ GHC.getLoc c)
           $ spaced
           $ fmap pretty
           $ c : cs
    else spacePrefixed $ fmap pretty $ c : cs
  eolCommentsArePrinted
#else
printCommentOnSameLine (commentsOnSameLine -> (c:cs)) = do
  col <- gets psColumn
  if col == 0
    then indentedWithFixedLevel
           (fromIntegral $ GHC.srcSpanStartCol $ GHC.anchor $ GHC.getLoc c)
           $ spaced
           $ fmap pretty
           $ c : cs
    else spacePrefixed $ fmap pretty $ c : cs
  eolCommentsArePrinted
#endif
printCommentOnSameLine _ = return ()

-- | Prints comments that are after the given AST node.
printCommentsAfter :: NodeComments -> Printer ()
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
printCommentsAfter p =
  case commentsAfter p of
    [] -> return ()
    xs -> do
      isThereCommentsOnSameLine <- gets psEolComment
      unless isThereCommentsOnSameLine newline
      forM_ xs $ \(GHC.L loc c) -> do
        let col =
              fromIntegral
                $ GHC.srcSpanStartCol (GHC.epaLocationRealSrcSpan loc) - 1
        indentedWithFixedLevel col $ pretty c
        eolCommentsArePrinted
#else
printCommentsAfter p =
  case commentsAfter p of
    [] -> return ()
    xs -> do
      isThereCommentsOnSameLine <- gets psEolComment
      unless isThereCommentsOnSameLine newline
      forM_ xs $ \(GHC.L loc c) -> do
        let col = fromIntegral $ GHC.srcSpanStartCol (GHC.anchor loc) - 1
        indentedWithFixedLevel col $ pretty c
        eolCommentsArePrinted
#endif
fromGenLocated :: (CommentExtraction l) => GHC.GenLocated l a -> WithComments a
fromGenLocated (GHC.L l a) = WithComments (nodeComments l) a

fromEpAnn :: GHC.EpAnn a -> b -> WithComments b
fromEpAnn ann = WithComments (NodeComments.fromEpAnn ann)

mkWithComments :: a -> WithComments a
mkWithComments = WithComments mempty

getNode :: WithComments a -> a
getNode = node

getComments :: WithComments a -> NodeComments
getComments = comments

flattenComments :: WithComments (WithComments a) -> WithComments a
flattenComments (WithComments outerComments (WithComments innerComments node)) =
  WithComments (outerComments <> innerComments) node

addComments :: NodeComments -> WithComments a -> WithComments a
addComments extra (WithComments current node) =
  WithComments (extra <> current) node
