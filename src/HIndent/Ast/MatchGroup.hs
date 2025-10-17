module HIndent.Ast.MatchGroup
  ( MatchGroup
  , mkExprMatchGroup
  , mkCmdMatchGroup
  , hasMatches
  ) where

import qualified GHC.Hs as GHC
import HIndent.Ast.Match (Match, mkCmdMatch, mkExprMatch)
import HIndent.Ast.WithComments
  ( WithComments
  , fromGenLocated
  , getComments
  , getNode
  , prettyWith
  )
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty)
import HIndent.Pretty.Combinators (lined)
import HIndent.Pretty.NodeComments (CommentExtraction(..))

newtype MatchGroup =
  MatchGroup (WithComments [WithComments Match])

instance CommentExtraction MatchGroup where
  nodeComments (MatchGroup alts) = getComments alts

instance Pretty MatchGroup where
  pretty' (MatchGroup alts) = prettyWith alts (lined . fmap pretty)

mkExprMatchGroup ::
     GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> MatchGroup
mkExprMatchGroup =
  MatchGroup
    . fmap (fmap (fmap mkExprMatch . fromGenLocated))
    . fromGenLocated
    . GHC.mg_alts

mkCmdMatchGroup :: GHC.MatchGroup GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> MatchGroup
mkCmdMatchGroup =
  MatchGroup
    . fmap (fmap (fmap mkCmdMatch . fromGenLocated))
    . fromGenLocated
    . GHC.mg_alts

hasMatches :: MatchGroup -> Bool
hasMatches (MatchGroup alts) = not $ null $ getNode alts
