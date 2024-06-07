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
