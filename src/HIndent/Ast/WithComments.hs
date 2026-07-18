{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module HIndent.Ast.WithComments
  ( WithComments
  , prettyWith
  , mkWithCommentsFromGenLocated
  , mkWithCommentsFromEpaLocated
  , fromEpAnn
  , mkWithComments
  , getNode
  , getComments
  , addComments
  , flattenComments
  ) where

import Control.Monad
import Control.Monad.RWS
import qualified GHC.Parser.Annotation as GHC
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.Comment (Comment, getColumn)
import HIndent.Ast.IsGenLocatedLocation
  ( CommentGroup(..)
  , mkCommentGroupFromEpAnn
  )
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
import HIndent.Ast.IsGenLocatedLocation
  ( IsGenLocatedLocation(..)
  , mkCommentGroupFromEpaLocation
  )
#endif
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Printer

data WithComments a = WithComments
  { comments :: CommentGroup
  , node :: a
  } deriving (Foldable, Traversable, Eq)

instance Functor WithComments where
  fmap f WithComments {..} = WithComments comments (f node)

instance (Pretty a) => Pretty (WithComments a) where
  pretty withComments = prettyWith withComments pretty

-- | Prints comments included in the location information and then the
-- AST node body.
prettyWith :: WithComments a -> (a -> Printer ()) -> Printer ()
prettyWith WithComments {..} f = do
  printCommentsBefore comments
  f node
  printCommentOnSameLine comments
  printCommentsAfter comments

-- | Prints comments that are before the given AST node.
printCommentsBefore :: CommentGroup -> Printer ()
printCommentsBefore p =
  forM_ (commentsBefore p) $ \comment -> do
    indentedWithFixedLevel (getCommentColumn comment) $ pretty comment
    newline

-- | Prints comments that are on the same line as the given AST node.
printCommentOnSameLine :: CommentGroup -> Printer ()
printCommentOnSameLine (commentsOnSameLine -> (c:cs)) = do
  col <- gets psColumn
  if col == 0
    then indentedWithFixedLevel (getCommentColumn c)
           $ spaced
           $ fmap pretty
           $ c : cs
    else spacePrefixed $ fmap pretty $ c : cs
  eolCommentsArePrinted
printCommentOnSameLine _ = return ()

-- | Prints comments that are after the given AST node.
printCommentsAfter :: CommentGroup -> Printer ()
printCommentsAfter p =
  case commentsAfter p of
    [] -> return ()
    xs -> do
      isThereCommentsOnSameLine <- gets psEolComment
      unless isThereCommentsOnSameLine newline
      forM_ xs $ \comment -> do
        indentedWithFixedLevel (getCommentColumn comment) $ pretty comment
        eolCommentsArePrinted
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkWithCommentsFromGenLocated ::
     IsGenLocatedLocation l => GHC.GenLocated l a -> WithComments a
mkWithCommentsFromGenLocated (GHC.L ann a) =
  WithComments (mkCommentGroupFromGenLocatedLocation ann) a

mkWithCommentsFromEpaLocated ::
     GHC.GenLocated GHC.EpaLocation a -> WithComments a
mkWithCommentsFromEpaLocated (GHC.L ann a) =
  WithComments (mkCommentGroupFromEpaLocation ann) a
#else
mkWithCommentsFromGenLocated ::
     GHC.GenLocated (GHC.SrcSpanAnn' (GHC.EpAnn ann)) a -> WithComments a
mkWithCommentsFromGenLocated (GHC.L ann a) =
  WithComments (mkCommentGroupFromEpAnn $ GHC.ann ann) a

mkWithCommentsFromEpaLocated ::
     GHC.GenLocated (GHC.SrcSpanAnn' (GHC.EpAnn ann)) a -> WithComments a
mkWithCommentsFromEpaLocated = mkWithCommentsFromGenLocated
#endif
fromEpAnn :: GHC.EpAnn a -> b -> WithComments b
fromEpAnn ann = WithComments (mkCommentGroupFromEpAnn ann)

mkWithComments :: a -> WithComments a
mkWithComments = WithComments mempty

getNode :: WithComments a -> a
getNode = node

getComments :: WithComments a -> CommentGroup
getComments = comments

flattenComments :: WithComments (WithComments a) -> WithComments a
flattenComments (WithComments outerComments (WithComments innerComments node)) =
  WithComments (outerComments <> innerComments) node

addComments :: CommentGroup -> WithComments a -> WithComments a
addComments extra (WithComments current node) =
  WithComments (extra <> current) node

getCommentColumn :: Comment -> Int
getCommentColumn = getColumn
