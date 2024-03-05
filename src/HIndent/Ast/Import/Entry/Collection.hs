{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Import.Entry.Collection
  ( ImportEntryCollection(..)
  , mkImportEntries
  ) where

import           Control.Monad
import qualified GHC.Hs                               as GHC
import qualified GHC.Types.SrcLoc                     as GHC
import           HIndent.Ast.Import.ImportingOrHiding
import           HIndent.Ast.NodeComments
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments

data ImportEntryCollection = ImportEntryCollection
  { entries :: GHC.GenLocated GHC.SrcSpanAnnL [GHC.LIE GHC.GhcPs]
  , kind    :: ImportingOrHiding
  }

instance CommentExtraction ImportEntryCollection where
  nodeComments ImportEntryCollection {} = NodeComments [] [] []

instance Pretty ImportEntryCollection where
  pretty' ImportEntryCollection {..} = do
    when (kind == Hiding) $ string " hiding"
    (space >> printCommentsAnd entries (hTuple . fmap pretty)) <-|>
      (newline >>
       indentedBlock (printCommentsAnd entries (vTuple . fmap pretty)))

mkImportEntries :: GHC.ImportDecl GHC.GhcPs -> Maybe ImportEntryCollection
mkImportEntries GHC.ImportDecl {..} =
  case ideclHiding of
    Nothing -> Nothing
    Just (False, xs) ->
      Just $ ImportEntryCollection {entries = xs, kind = Importing}
    Just (True, xs) ->
      Just $ ImportEntryCollection {entries = xs, kind = Hiding}
