module HIndent.Ast.Declaration.Default
  ( DefaultDeclaration
  , mkDefaultDeclaration
  ) where

import qualified GHC.Hs as GHC
import HIndent.Ast.NodeComments
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

newtype DefaultDeclaration =
  DefaultDeclaration [GHC.LHsType GHC.GhcPs]

instance CommentExtraction DefaultDeclaration where
  nodeComments DefaultDeclaration {} = NodeComments [] [] []

instance Pretty DefaultDeclaration where
  pretty' (DefaultDeclaration xs) =
    spaced [string "default", hTuple $ fmap pretty xs]

mkDefaultDeclaration :: GHC.DefaultDecl GHC.GhcPs -> DefaultDeclaration
mkDefaultDeclaration (GHC.DefaultDecl _ xs) = DefaultDeclaration xs
