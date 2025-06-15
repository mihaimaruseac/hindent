{-# LANGUAGE CPP #-}

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
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkDefaultDeclaration (GHC.DefaultDecl _ _ xs) = DefaultDeclaration xs
#else
mkDefaultDeclaration (GHC.DefaultDecl _ xs) = DefaultDeclaration xs
#endif
