module HIndent.Ast.Type.Literal
  ( Literal
  , mkLiteral
  ) where

import qualified GHC.Data.FastString as GHC
import HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import Text.Show.Unicode

data Literal
  = Numeric Integer
  | String String
  | Character Char

instance CommentExtraction Literal where
  nodeComments _ = NodeComments [] [] []

instance Pretty Literal where
  pretty' (Numeric n) = string $ show n
  pretty' (String s) = string $ ushow s
  pretty' (Character c) = string $ ushow c

mkLiteral :: GHC.HsTyLit GHC.GhcPs -> Literal
mkLiteral (GHC.HsNumTy _ n) = Numeric n
mkLiteral (GHC.HsStrTy _ s) = String (GHC.unpackFS s)
mkLiteral (GHC.HsCharTy _ c) = Character c
