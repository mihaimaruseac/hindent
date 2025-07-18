{-# LANGUAGE CPP #-}

module HIndent.Ast.Expression.OverloadedLabel
  ( OverloadedLabel
  , mkOverloadedLabel
  ) where

import qualified GHC.Data.FastString as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

newtype OverloadedLabel =
  OverloadedLabel String

instance CommentExtraction OverloadedLabel where
  nodeComments OverloadedLabel {} = emptyNodeComments

instance Pretty OverloadedLabel where
  pretty' (OverloadedLabel s) = string "#" >> string s

mkOverloadedLabel :: GHC.FastString -> OverloadedLabel
mkOverloadedLabel fs = OverloadedLabel $ GHC.unpackFS fs
