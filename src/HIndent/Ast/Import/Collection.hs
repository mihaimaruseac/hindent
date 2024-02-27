{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Import.Collection
  ( ImportCollection
  , mkImportCollection
  ) where

import qualified GHC.Hs as GHC
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC

newtype ImportCollection =
  ImportCollection [GHC.LImportDecl GHC.GhcPs]

mkImportCollection :: GHC.HsModule' -> ImportCollection
mkImportCollection GHC.HsModule {..} = ImportCollection hsmodImports
