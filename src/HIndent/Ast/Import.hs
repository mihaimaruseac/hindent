{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Import
  ( Import
  , mkImport
  , sortByName
  ) where

import           Control.Monad
import           Data.Function
import           Data.List
import qualified GHC.Unit                            as GHC
import           HIndent.Applicative
import           HIndent.Ast.Import.Entry.Collection
import           HIndent.Ast.Import.Qualification
import           HIndent.Ast.NodeComments
import           HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs  as GHC
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments

data Import = Import
  { moduleName    :: String
  , isSafe        :: Bool
  , isBoot        :: Bool
  , qualification :: Qualification
  , packageName   :: Maybe String
  , importEntries :: Maybe (WithComments ImportEntryCollection)
  }

instance CommentExtraction Import where
  nodeComments Import {} = NodeComments [] [] []

instance Pretty Import where
  pretty' Import {..} = do
    string "import "
    when isBoot $ string "{-# SOURCE #-} "
    when isSafe $ string "safe "
    unless (qualification == NotQualified) $ string "qualified "
    whenJust packageName $ \name -> string name >> space
    string moduleName
    case qualification of
      QualifiedAs name -> string " as " >> string name
      _                -> pure ()
    whenJust importEntries pretty

mkImport :: GHC.ImportDecl GHC.GhcPs -> Import
mkImport decl@GHC.ImportDecl {..} = Import {..}
  where
    moduleName = showOutputable ideclName
    isSafe = ideclSafe
    isBoot = ideclSource == GHC.IsBoot
    qualification =
      case (ideclQualified, ideclAs) of
        (GHC.NotQualified, _) -> NotQualified
        (_, Nothing)          -> FullyQualified
        (_, Just name)        -> QualifiedAs $ showOutputable name
    packageName = fmap showOutputable ideclPkgQual
    importEntries = mkImportEntryCollection decl

sortByName :: [WithComments Import] -> [WithComments Import]
sortByName = fmap sortExplicitImportsInDecl . sortByModuleName

-- | This function sorts import declarations by their module names.
sortByModuleName :: [WithComments Import] -> [WithComments Import]
sortByModuleName = sortBy (compare `on` moduleName . getNode)

-- | This function sorts explicit imports in the given import declaration
-- by their names.
sortExplicitImportsInDecl :: WithComments Import -> WithComments Import
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
sortExplicitImportsInDecl (L l d@ImportDecl {ideclImportList = Just (x, imports)}) =
  L l d {ideclImportList = Just (x, sorted)}
  where
    sorted = fmap (fmap sortVariants . sortExplicitImports) imports
#else
sortExplicitImportsInDecl = fmap f
  where
    f (Import {importEntries = Just xs, ..}) =
      Import {importEntries = Just sorted, ..}
      where
        sorted = fmap sortEntriesByName xs
    f x = x
#endif
