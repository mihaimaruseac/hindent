module HIndent.Ast.MatchGroup
  ( MatchGroup
  , mkCmdMatchGroup
  ) where

import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty (Pretty)

data MatchGroup

instance Pretty MatchGroup


mkCmdMatchGroup ::
     GHC.MatchGroup GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> MatchGroup
