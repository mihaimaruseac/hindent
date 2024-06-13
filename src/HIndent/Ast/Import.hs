{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Import
  ( Import
  , mkImport
  , sortByName
  ) where

import Control.Monad
import Data.Function
import Data.List
import qualified GHC.Types.SourceText as GHC
import qualified GHC.Unit as GHC
import HIndent.Applicative
import HIndent.Ast.Import.Entry.Collection
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

data Qualification = Qualification
  { qualifiedAs :: Maybe (GHC.XRec GHC.GhcPs GHC.ModuleName)
  , position :: QualificationPosition
  } deriving (Eq)

data Import = Import
  { moduleName :: GHC.XRec GHC.GhcPs GHC.ModuleName
  , isSafe :: Bool
  , isBoot :: Bool
  , qualification :: Maybe Qualification
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
    when (fmap position qualification == Just Pre) $ string "qualified "
    whenJust packageName $ \name -> pretty name >> space
    pretty moduleName
    when (fmap position qualification == Just Post) $ string " qualified"
    case qualification of
      Just Qualification {qualifiedAs = Just name} ->
        string " as " >> pretty name
      _ -> pure ()
    whenJust importEntries pretty

mkImport :: GHC.ImportDecl GHC.GhcPs -> Import
mkImport decl@GHC.ImportDecl {..} = Import {..}
  where
    moduleName = ideclName
    isSafe = ideclSafe
    isBoot = ideclSource == GHC.IsBoot
    qualification =
      case (ideclQualified, ideclAs, ideclQualified) of
        (GHC.NotQualified, _, _) -> Nothing
        (_, Nothing, GHC.QualifiedPre) ->
          Just Qualification {qualifiedAs = Nothing, position = Pre}
        (_, Nothing, GHC.QualifiedPost) ->
          Just Qualification {qualifiedAs = Nothing, position = Post}
        (_, Just name, GHC.QualifiedPre) ->
          Just Qualification {qualifiedAs = Just name, position = Pre}
        (_, Just name, GHC.QualifiedPost) ->
          Just Qualification {qualifiedAs = Just name, position = Post}
    packageName = GHC.getPackageName decl
    importEntries = mkImportEntryCollection decl

sortByName :: [WithComments Import] -> [WithComments Import]
sortByName = fmap sortExplicitImportsInDecl . sortByModuleName

-- | This function sorts import declarations by their module names.
sortByModuleName :: [WithComments Import] -> [WithComments Import]
sortByModuleName = sortBy (compare `on` showOutputable . moduleName . getNode)

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
