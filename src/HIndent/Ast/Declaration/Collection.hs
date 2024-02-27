{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Collection
  ( DeclarationCollection
  , mkDeclarationCollection
  ) where

import qualified GHC.Hs as GHC
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC

newtype DeclarationCollection =
  DeclarationCollection [GHC.LHsDecl GHC.GhcPs]

mkDeclarationCollection :: GHC.HsModule' -> DeclarationCollection
mkDeclarationCollection GHC.HsModule {..} = DeclarationCollection hsmodDecls
