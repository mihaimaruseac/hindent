{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Comment
  ( Comment
  , getColumn
  , mkComment
  , mkCommentFromToken
  ) where

import qualified Data.Text as Text
import qualified GHC.Hs as GHC
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.TextValue
import qualified HIndent.GhcLibParserWrapper.GHC.Parser.Annotation as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments (CommentExtraction(..))

data Comment
  = Line
      { text :: TextValue
      , column :: Int
      }
  | Block
      { text :: TextValue
      , column :: Int
      }
  deriving (Eq, Show)

instance CommentExtraction Comment where
  nodeComments _ = mempty

instance Pretty Comment where
  pretty' Line {..} = pretty text
  pretty' Block {..} =
    case Text.lines $ toText text of
      [] -> pure ()
      [x] -> string $ Text.unpack x
      (x:xs) -> do
        string $ Text.unpack x
        newline
        -- 'indentedWithFixedLevel 0' is used because a 'BlockComment'
        -- contains indent spaces for all lines except the first one.
        indentedWithFixedLevel 0 $ lined $ fmap (string . Text.unpack) xs

mkComment :: GHC.LEpaComment -> Comment
mkComment comment@(GHC.L _ (GHC.EpaComment (GHC.EpaLineComment text) _)) =
  Line {text = mkTextValueFromString text, column = getColumn' comment}
mkComment comment@(GHC.L _ (GHC.EpaComment (GHC.EpaBlockComment text) _)) =
  Block {text = mkTextValueFromString text, column = getColumn' comment}
mkComment comment@(GHC.L _ _) =
  Line {text = mkTextValueFromString "", column = getColumn' comment}

mkCommentFromToken :: GHC.EpaCommentTok -> Comment
mkCommentFromToken (GHC.EpaLineComment text) =
  Line {text = mkTextValueFromString text, column = 0}
mkCommentFromToken (GHC.EpaBlockComment text) =
  Block {text = mkTextValueFromString text, column = 0}
mkCommentFromToken _ = Line {text = mkTextValueFromString "", column = 0}

getColumn :: Comment -> Int
getColumn = column

getColumn' :: GHC.LEpaComment -> Int
getColumn' =
  subtract 1 . GHC.srcSpanStartCol . GHC.epaLocationToRealSrcSpan . GHC.getLoc
