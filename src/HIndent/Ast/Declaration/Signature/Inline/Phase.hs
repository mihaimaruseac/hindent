{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Signature.Inline.Phase
  ( InlinePhase
  , mkInlinePhase
  ) where

import qualified Data.Text as Text
import qualified GHC.Types.Basic as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators

data BeforeOrAfter
  = Before
  | After

data InlinePhase = InlinePhase
  { beforeOrAfter :: BeforeOrAfter
  , phase :: Int
  }

instance Pretty InlinePhase where
  pretty InlinePhase {beforeOrAfter = Before, ..} =
    brackets (string $ Text.pack $ '~' : show phase)
  pretty InlinePhase {beforeOrAfter = After, ..} =
    brackets (string $ Text.pack $ show phase)

mkInlinePhase :: GHC.Activation -> Maybe InlinePhase
mkInlinePhase (GHC.ActiveBefore _ phase) = Just $ InlinePhase Before phase
mkInlinePhase (GHC.ActiveAfter _ phase) = Just $ InlinePhase After phase
mkInlinePhase _ = Nothing
