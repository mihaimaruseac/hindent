module HIndent.Ast.LocalBinds.ImplicitBindings
  ( ImplicitBindings
  , mkImplicitBindings
  ) where

import HIndent.Ast.LocalBinds.ImplicitBinding
  ( ImplicitBinding
  , mkImplicitBinding
  )
import HIndent.Ast.WithComments (WithComments, mkWithCommentsFromGenLocated)
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty (Pretty(..))
import HIndent.Pretty.Combinators

newtype ImplicitBindings = ImplicitBindings
  { bindings :: [WithComments ImplicitBinding]
  }

instance Pretty ImplicitBindings where
  pretty (ImplicitBindings xs) = lined $ fmap pretty xs

mkImplicitBindings :: GHC.HsIPBinds GHC.GhcPs -> ImplicitBindings
mkImplicitBindings (GHC.IPBinds _ xs) =
  ImplicitBindings
    {bindings = fmap (fmap mkImplicitBinding . mkWithCommentsFromGenLocated) xs}
