module HIndent.Ast.LocalBinds.ImplicitBindings
  ( ImplicitBindings
  , mkImplicitBindings
  ) where

import HIndent.Ast.LocalBinds.ImplicitBinding
  ( ImplicitBinding
  , mkImplicitBinding
  )
import HIndent.Ast.NodeComments (NodeComments(..))
import HIndent.Ast.WithComments (WithComments, fromGenLocated)
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty)
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

newtype ImplicitBindings = ImplicitBindings
  { bindings :: [WithComments ImplicitBinding]
  }

instance CommentExtraction ImplicitBindings where
  nodeComments _ = NodeComments [] [] []

instance Pretty ImplicitBindings where
  pretty' (ImplicitBindings xs) = lined $ fmap pretty xs

mkImplicitBindings :: GHC.HsIPBinds GHC.GhcPs -> ImplicitBindings
mkImplicitBindings (GHC.IPBinds _ xs) =
  ImplicitBindings
    {bindings = fmap (fmap mkImplicitBinding . fromGenLocated) xs}
