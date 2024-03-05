{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Import.Entry.Collection
  ( ImportEntryCollection
  , mkImportEntryCollection
  , sortEntriesByName
  ) where

import Control.Monad
import qualified GHC.Hs as GHC
import HIndent.Ast.Import.Entry
import HIndent.Ast.Import.ImportingOrHiding
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data ImportEntryCollection = ImportEntryCollection
  { entries :: [WithComments ImportEntry]
  , kind :: ImportingOrHiding
  }

instance CommentExtraction ImportEntryCollection where
  nodeComments ImportEntryCollection {} = NodeComments [] [] []

instance Pretty ImportEntryCollection where
  pretty' ImportEntryCollection {..} = do
    when (kind == Hiding) $ string " hiding"
    (space >> hTuple (fmap pretty entries))
      <-|> (newline >> indentedBlock (vTuple $ fmap pretty entries))

mkImportEntryCollection ::
     GHC.ImportDecl GHC.GhcPs -> Maybe (WithComments ImportEntryCollection)
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
mkImportEntryCollection GHC.ImportDecl {..} =
  case ideclImportList of
    Nothing -> Nothing
    Just (GHC.Exactly, xs) ->
      Just
        $ fmap (\entries -> ImportEntryCollection {kind = Importing, ..})
        $ fromGenLocated
        $ fmap (fmap (fmap mkImportEntry . fromGenLocated)) xs
    Just (GHC.EverythingBut, xs) ->
      Just
        $ fmap (\entries -> ImportEntryCollection {kind = Hiding, ..})
        $ fromGenLocated
        $ fmap (fmap (fmap mkImportEntry . fromGenLocated)) xs
#else
mkImportEntryCollection GHC.ImportDecl {..} =
  case ideclHiding of
    Nothing -> Nothing
    Just (False, xs) ->
      Just
        $ fmap (\entries -> ImportEntryCollection {kind = Importing, ..})
        $ fromGenLocated
        $ fmap (fmap (fmap mkImportEntry . fromGenLocated)) xs
    Just (True, xs) ->
      Just
        $ fmap (\entries -> ImportEntryCollection {kind = Hiding, ..})
        $ fromGenLocated
        $ fmap (fmap (fmap mkImportEntry . fromGenLocated)) xs
#endif
sortEntriesByName :: ImportEntryCollection -> ImportEntryCollection
sortEntriesByName ImportEntryCollection {..} =
  ImportEntryCollection {entries = sortVariantsAndExplicitImports entries, ..}
