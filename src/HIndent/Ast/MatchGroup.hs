{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module HIndent.Ast.MatchGroup
  ( MatchGroup(..)
  , mkMatchGroup
  , matchGroupAlternatives
  , matchGroupMatches
  ) where

import qualified GHC.Hs as GHC
import qualified GHC.Types.SrcLoc as SrcLoc
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty, printCommentsAnd)
import HIndent.Pretty.Combinators (lined)
import HIndent.Pretty.NodeComments (CommentExtraction(..), emptyNodeComments)

type LHsExprPs = SrcLoc.GenLocated GHC.SrcSpanAnnA (GHC.HsExpr GHC.GhcPs)

type LHsCmdPs = SrcLoc.GenLocated GHC.SrcSpanAnnA (GHC.HsCmd GHC.GhcPs)

newtype MatchGroup body = MatchGroup
  { toGhcMatchGroup :: GHC.MatchGroup GHC.GhcPs body
  }

instance CommentExtraction (MatchGroup body) where
  nodeComments _ = emptyNodeComments

instance Pretty (MatchGroup LHsExprPs) where
  pretty' (MatchGroup GHC.MG {..}) =
    printCommentsAnd mg_alts (lined . fmap pretty)

instance Pretty (MatchGroup LHsCmdPs) where
  pretty' (MatchGroup GHC.MG {..}) =
    printCommentsAnd mg_alts (lined . fmap pretty)

mkMatchGroup :: GHC.MatchGroup GHC.GhcPs body -> MatchGroup body
mkMatchGroup = MatchGroup

matchGroupAlternatives ::
     MatchGroup body -> GHC.XRec GHC.GhcPs [GHC.LMatch GHC.GhcPs body]
matchGroupAlternatives (MatchGroup GHC.MG {..}) = mg_alts

matchGroupMatches :: MatchGroup body -> [GHC.LMatch GHC.GhcPs body]
matchGroupMatches = SrcLoc.unLoc . matchGroupAlternatives
