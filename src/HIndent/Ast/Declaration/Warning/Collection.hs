{-# LANGUAGE CPP #-}

module HIndent.Ast.Declaration.Warning.Collection
  ( WarningCollection
  , mkWarningCollection
  ) where

import HIndent.Ast.Declaration.Warning
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators

newtype WarningCollection =
  WarningCollection [WithComments WarningDeclaration]

instance Pretty WarningCollection where
  pretty (WarningCollection xs) = lined $ fmap pretty xs

mkWarningCollection :: GHC.WarnDecls GHC.GhcPs -> WarningCollection
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
mkWarningCollection (GHC.Warnings _ xs) =
  WarningCollection
    $ fmap (fmap mkWarningDeclaration . mkWithCommentsFromGenLocated) xs
#else
mkWarningCollection (GHC.Warnings _ _ xs) =
  WarningCollection
    $ fmap (fmap mkWarningDeclaration . mkWithCommentsFromGenLocated) xs
#endif
