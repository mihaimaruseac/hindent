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
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty)
import HIndent.Pretty.NodeComments (CommentExtraction(..), emptyNodeComments)
import HIndent.Printer (Printer)

type LHsExprPs = SrcLoc.GenLocated GHC.SrcSpanAnnA (GHC.HsExpr GHC.GhcPs)

type LHsCmdPs = SrcLoc.GenLocated GHC.SrcSpanAnnA (GHC.HsCmd GHC.GhcPs)

newtype MatchGroup body = MatchGroup
  { toGhcMatchGroup :: GHC.MatchGroup GHC.GhcPs body
  }

instance CommentExtraction (MatchGroup body) where
  nodeComments _ = emptyNodeComments

class MatchGroupPrintable body where
  prettyMatchGroup :: GHC.MatchGroup GHC.GhcPs body -> Printer ()

instance MatchGroupPrintable LHsExprPs where
  prettyMatchGroup = pretty

instance MatchGroupPrintable LHsCmdPs where
  prettyMatchGroup = pretty

instance MatchGroupPrintable body => Pretty (MatchGroup body) where
  pretty' (MatchGroup mg) = prettyMatchGroup mg

mkMatchGroup :: GHC.MatchGroup GHC.GhcPs body -> MatchGroup body
mkMatchGroup = MatchGroup

matchGroupAlternatives ::
     MatchGroup body -> GHC.XRec GHC.GhcPs [GHC.LMatch GHC.GhcPs body]
matchGroupAlternatives (MatchGroup GHC.MG {..}) = mg_alts

matchGroupMatches :: MatchGroup body -> [GHC.LMatch GHC.GhcPs body]
matchGroupMatches = SrcLoc.unLoc . matchGroupAlternatives
