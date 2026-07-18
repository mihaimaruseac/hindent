module HIndent.Ast.Declaration.Bind
  ( Bind
  , mkBind
  ) where

import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty (Pretty)

data Bind

instance Pretty Bind

mkBind :: GHC.HsBind GHC.GhcPs -> Bind
