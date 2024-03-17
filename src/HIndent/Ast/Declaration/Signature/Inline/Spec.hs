{-# LANGUAGE CPP #-}

module HIndent.Ast.Declaration.Signature.Inline.Spec
  ( InlineSpec
  , mkInlineSpec
  ) where

import qualified GHC.Types.Basic as GHC
import HIndent.Ast.NodeComments
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data InlineSpec
  = Inline
  | Inlinable
  | NoInline
  | Opaque

instance CommentExtraction InlineSpec where
  nodeComments Inline = NodeComments [] [] []
  nodeComments Inlinable = NodeComments [] [] []
  nodeComments NoInline = NodeComments [] [] []
  nodeComments Opaque = NodeComments [] [] []

instance Pretty InlineSpec where
  pretty' Inline = string "INLINE"
  pretty' Inlinable = string "INLINABLE"
  pretty' NoInline = string "NOINLINE"
  pretty' Opaque = string "OPAQUE"

mkInlineSpec :: GHC.InlineSpec -> InlineSpec
mkInlineSpec GHC.Inline {} = Inline
mkInlineSpec GHC.Inlinable {} = Inlinable
mkInlineSpec GHC.NoInline {} = NoInline
mkInlineSpec GHC.NoUserInlinePrag = error "NoUserInlinePrag is not supported"
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
mkInlineSpec GHC.Opaque {} = Opaque
#endif
