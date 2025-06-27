{-# LANGUAGE CPP #-}

module HIndent.Ast.Declaration.Instance.Class.OverlapMode
  ( OverlapMode
  , mkOverlapMode
  ) where

import qualified GHC.Types.Basic as GHC
import HIndent.Ast.NodeComments
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators.String
import HIndent.Pretty.NodeComments

data OverlapMode
  = Overlappable
  | Overlapping
  | Overlaps
  | Incoherent

instance CommentExtraction OverlapMode where
  nodeComments Overlappable = NodeComments [] [] []
  nodeComments Overlapping = NodeComments [] [] []
  nodeComments Overlaps = NodeComments [] [] []
  nodeComments Incoherent = NodeComments [] [] []

instance Pretty OverlapMode where
  pretty' Overlappable = string "{-# OVERLAPPABLE #-}"
  pretty' Overlapping = string "{-# OVERLAPPING #-}"
  pretty' Overlaps = string "{-# OVERLAPS #-}"
  pretty' Incoherent = string "{-# INCOHERENT #-}"

mkOverlapMode :: GHC.OverlapMode -> OverlapMode
mkOverlapMode GHC.NoOverlap {} =
  error "This AST node should never appear in the tree"
mkOverlapMode GHC.Overlappable {} = Overlappable
mkOverlapMode GHC.Overlapping {} = Overlapping
mkOverlapMode GHC.Overlaps {} = Overlaps
mkOverlapMode GHC.Incoherent {} = Incoherent
#if MIN_VERSION_ghc_lib_parser(9, 8, 1)
-- There is no pragma named `{-# NONCANONICAL #-}`. Instead, GHC internally
-- treats `{-# INCOHERENT #-}` as enabling a noncanonical overlap mode
-- if the `-fno-specialise-incoherents` flag is specified.
-- For details, see:
-- https://hackage-content.haskell.org/package/ghc-9.12.2/docs/src/GHC.Types.Basic.html#NonCanonical
mkOverlapMode GHC.NonCanonical {} = Incoherent
#endif
