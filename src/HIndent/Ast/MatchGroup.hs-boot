module HIndent.Ast.MatchGroup
  ( MatchGroup
  , mkCmdMatchGroup
  ) where

import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty (Pretty)
import HIndent.Pretty.NodeComments

data MatchGroup

instance Pretty MatchGroup

instance CommentExtraction MatchGroup

mkCmdMatchGroup ::
     GHC.MatchGroup GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> MatchGroup
