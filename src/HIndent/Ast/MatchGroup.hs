{-# LANGUAGE FlexibleInstances #-}

module HIndent.Ast.MatchGroup
  ( MatchGroup(..)
  , MatchGroupExpr
  , MatchGroupCmd
  , mkExprMatchGroup
  , mkCmdMatchGroup
  , matchGroupAlternatives
  , matchGroupMatches
  ) where

import qualified GHC.Hs as GHC
import qualified GHC.Types.SrcLoc as SrcLoc
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

type LHsExprPs = SrcLoc.GenLocated GHC.SrcSpanAnnA (GHC.HsExpr GHC.GhcPs)

type LHsCmdPs = SrcLoc.GenLocated GHC.SrcSpanAnnA (GHC.HsCmd GHC.GhcPs)

newtype MatchGroup body = MatchGroup
  { alternatives :: WithComments [GHC.LMatch GHC.GhcPs body]
  }

type MatchGroupExpr = MatchGroup LHsExprPs

type MatchGroupCmd = MatchGroup LHsCmdPs

instance CommentExtraction (MatchGroup body) where
  nodeComments = getComments . alternatives

instance Pretty MatchGroupExpr where
  pretty' MatchGroup {alternatives = alts} =
    prettyWith alts (lined . fmap pretty)

instance Pretty MatchGroupCmd where
  pretty' MatchGroup {alternatives = alts} =
    prettyWith alts (lined . fmap pretty)

mkExprMatchGroup ::
     GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> MatchGroupExpr
mkExprMatchGroup = MatchGroup . fromGenLocated . GHC.mg_alts

mkCmdMatchGroup ::
     GHC.MatchGroup GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> MatchGroupCmd
mkCmdMatchGroup = MatchGroup . fromGenLocated . GHC.mg_alts

matchGroupAlternatives ::
     MatchGroup body -> WithComments [GHC.LMatch GHC.GhcPs body]
matchGroupAlternatives = alternatives

matchGroupMatches :: MatchGroup body -> [GHC.LMatch GHC.GhcPs body]
matchGroupMatches = getNode . alternatives
