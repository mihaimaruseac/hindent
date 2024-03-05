module HIndent.Ast.Import
  ( Import
  , mkImport
  , sortByName
  ) where

import           HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import           HIndent.Pretty
import           HIndent.Pretty.Import.Sort
import           HIndent.Pretty.NodeComments

newtype Import = Import
  { import' :: GHC.LImportDecl GHC.GhcPs
  }

instance CommentExtraction Import where
  nodeComments Import {} = NodeComments [] [] []

instance Pretty Import where
  pretty' (Import x) = pretty x

mkImport :: GHC.LImportDecl GHC.GhcPs -> Import
mkImport = Import

sortByName :: [Import] -> [Import]
sortByName = fmap Import . sortImportsByName . fmap import'
