module HIndent.Ast.Comment
  ( Comment
  , mkComment
  ) where

import qualified GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments (CommentExtraction(..))

data Comment
  = Line String
  | Block String
  deriving (Eq, Show)

instance CommentExtraction Comment where
  nodeComments _ = mempty

instance Pretty Comment where
  pretty' (Line c) = string c
  pretty' (Block c) =
    case lines c of
      [] -> pure ()
      [x] -> string x
      (x:xs) -> do
        string x
        newline
        -- 'indentedWithFixedLevel 0' is used because a 'BlockComment'
        -- contains indent spaces for all lines except the first one.
        indentedWithFixedLevel 0 $ lined $ fmap string xs

mkComment :: GHC.EpaCommentTok -> Comment
mkComment (GHC.EpaLineComment c) = Line c
mkComment (GHC.EpaBlockComment c) = Block c
mkComment _ = Line ""
