module HIndent.Ast.Type where

import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty.NodeComments
import {-# SOURCE #-} HIndent.Pretty

data Type

instance CommentExtraction Type
instance Pretty Type

mkType :: GHC.HsType GHC.GhcPs -> Type