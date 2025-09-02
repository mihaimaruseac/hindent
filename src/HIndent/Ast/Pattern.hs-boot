{-# LANGUAGE FlexibleInstances #-}

module HIndent.Ast.Pattern
  ( Pattern
  , mkPattern
  ) where

import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty.NodeComments
import {-# SOURCE #-} HIndent.Pretty

data Pattern

instance CommentExtraction Pattern
instance Pretty Pattern

mkPattern :: GHC.Pat GHC.GhcPs -> Pattern