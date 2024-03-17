{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Signature.Inline.Phase
  ( InlinePhase
  , mkInlinePhase
  ) where

import qualified GHC.Types.Basic as GHC
import HIndent.Ast.NodeComments
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data BeforeOrAfter
  = Before
  | After

data InlinePhase = InlinePhase
  { beforeOrAfter :: BeforeOrAfter
  , phase :: Int
  }

instance CommentExtraction InlinePhase where
  nodeComments InlinePhase {} = NodeComments [] [] []

instance Pretty InlinePhase where
  pretty' InlinePhase {beforeOrAfter = Before, ..} =
    brackets (string $ '~' : show phase)
  pretty' InlinePhase {beforeOrAfter = After, ..} =
    brackets (string $ show phase)

mkInlinePhase :: GHC.Activation -> Maybe InlinePhase
mkInlinePhase (GHC.ActiveBefore _ phase) = Just $ InlinePhase Before phase
mkInlinePhase (GHC.ActiveAfter _ phase) = Just $ InlinePhase After phase
mkInlinePhase _ = Nothing
