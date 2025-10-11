{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.MatchGroup
  ( MatchGroup(..)
  , mkMatchGroup
  , matchGroupAlternatives
  , matchGroupMatches
  ) where

import qualified GHC.Hs as GHC
import qualified GHC.Types.SrcLoc as SrcLoc
import HIndent.Pretty.NodeComments (CommentExtraction(..), emptyNodeComments)

newtype MatchGroup body = MatchGroup
  { toGhcMatchGroup :: GHC.MatchGroup GHC.GhcPs body
  }

instance CommentExtraction (MatchGroup body) where
  nodeComments _ = emptyNodeComments

mkMatchGroup :: GHC.MatchGroup GHC.GhcPs body -> MatchGroup body
mkMatchGroup = MatchGroup

matchGroupAlternatives ::
     MatchGroup body -> GHC.XRec GHC.GhcPs [GHC.LMatch GHC.GhcPs body]
matchGroupAlternatives (MatchGroup GHC.MG {..}) = mg_alts

matchGroupMatches :: MatchGroup body -> [GHC.LMatch GHC.GhcPs body]
matchGroupMatches = SrcLoc.unLoc . matchGroupAlternatives
