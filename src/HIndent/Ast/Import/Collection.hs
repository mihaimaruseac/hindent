{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Import.Collection
  ( ImportCollection
  , mkImportCollection
  , hasImports
  ) where

import           Control.Monad.RWS
import qualified GHC.Hs                             as GHC
import           HIndent.Ast.NodeComments
import           HIndent.Config
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.Import
import           HIndent.Pretty.Import.Sort
import           HIndent.Pretty.NodeComments
import           HIndent.Printer

newtype ImportCollection =
  ImportCollection [[GHC.LImportDecl GHC.GhcPs]]

instance CommentExtraction ImportCollection where
  nodeComments ImportCollection {} = NodeComments [] [] []

instance Pretty ImportCollection where
  pretty' (ImportCollection xs) =
    importDecls >>= blanklined . fmap outputImportGroup
    where
      outputImportGroup = lined . fmap pretty
      importDecls =
        gets (configSortImports . psConfig) >>= \case
          True  -> pure $ fmap sortImportsByName xs
          False -> pure xs

mkImportCollection :: GHC.HsModule' -> ImportCollection
mkImportCollection GHC.HsModule {..} =
  ImportCollection $ extractImports' hsmodImports

hasImports :: ImportCollection -> Bool
hasImports (ImportCollection xs) = not $ null xs
