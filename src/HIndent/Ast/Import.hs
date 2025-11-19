{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Import
  ( Import
  , mkImport
  , sortByName
  ) where

import Control.Monad
import Data.Function
import Data.List (sortBy)
import qualified GHC.Types.SourceText as GHC
import qualified GHC.Unit as GHC
import HIndent.Applicative
import HIndent.Ast.Import.Entry.Collection
import HIndent.Ast.Module.Name (ModuleName, mkModuleName)
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import qualified HIndent.GhcLibParserWrapper.GHC.Hs.ImpExp as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data QualificationPosition
  = Pre
  | Post
  deriving (Eq)

data Import = Import
  { moduleName :: WithComments ModuleName
  , isSafe :: Bool
  , isBoot :: Bool
  , qualifiedAs :: Maybe (WithComments ModuleName)
  , qualification :: Maybe QualificationPosition
  , packageName :: Maybe GHC.StringLiteral
  , importEntries :: Maybe (WithComments ImportEntryCollection)
  }

instance CommentExtraction Import where
  nodeComments Import {} = NodeComments [] [] []

instance Pretty Import where
  pretty' Import {..} = do
    string "import "
    when isBoot $ string "{-# SOURCE #-} "
    when isSafe $ string "safe "
    when (qualification == Just Pre) $ string "qualified "
    whenJust packageName $ \name -> pretty name >> space
    pretty moduleName
    when (qualification == Just Post) $ string " qualified"
    case qualifiedAs of
      Just name -> string " as " >> pretty name
      _ -> pure ()
    whenJust importEntries pretty

mkImport :: GHC.ImportDecl GHC.GhcPs -> Import
mkImport decl@GHC.ImportDecl {..} = Import {..}
  where
    moduleName = mkModuleName <$> fromGenLocated ideclName
    isSafe = ideclSafe
    isBoot = ideclSource == GHC.IsBoot
    qualification =
      case ideclQualified of
        GHC.NotQualified -> Nothing
        GHC.QualifiedPre -> Just Pre
        GHC.QualifiedPost -> Just Post
    qualifiedAs = fmap mkModuleName . fromGenLocated <$> ideclAs
    packageName = GHC.getPackageName decl
    importEntries = mkImportEntryCollection decl

sortByName :: [WithComments Import] -> [WithComments Import]
sortByName = fmap sortExplicitImportsInDecl . sortByModuleName

-- | This function sorts import declarations by their module names.
sortByModuleName :: [WithComments Import] -> [WithComments Import]
sortByModuleName = sortBy (compare `on` getNode . moduleName . getNode)

-- | This function sorts explicit imports in the given import declaration
-- by their names.
sortExplicitImportsInDecl :: WithComments Import -> WithComments Import
sortExplicitImportsInDecl = fmap f
  where
    f (Import {importEntries = Just xs, ..}) =
      Import {importEntries = Just sorted, ..}
      where
        sorted = fmap sortEntriesByName xs
    f x = x
