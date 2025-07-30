{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Import.Entry
  ( ImportEntry
  , mkImportEntry
  , sortVariantsAndExplicitImports
  ) where

import Data.Function
import Data.List (sortBy)
import qualified GHC.Hs as GHC
import HIndent.Ast.Name.ImportExport
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data ImportEntry
  = SingleIdentifier (WithComments ImportExportName)
  | WithSpecificConstructors
      { name :: WithComments ImportExportName
      , constructors :: [WithComments ImportExportName]
      }
  | WithAllConstructors (WithComments ImportExportName)

instance CommentExtraction ImportEntry where
  nodeComments _ = NodeComments [] [] []

instance Pretty ImportEntry where
  pretty' (SingleIdentifier wrapped) = pretty wrapped
  pretty' (WithAllConstructors wrapped) = pretty wrapped >> string "(..)"
  pretty' WithSpecificConstructors {..} =
    pretty name >> hFillingTuple (fmap pretty constructors)

mkImportEntry :: GHC.IE GHC.GhcPs -> ImportEntry
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkImportEntry (GHC.IEVar _ name _) =
  SingleIdentifier $ mkImportExportName <$> fromGenLocated name
mkImportEntry (GHC.IEThingAbs _ name _) =
  SingleIdentifier $ mkImportExportName <$> fromGenLocated name
mkImportEntry (GHC.IEThingAll _ name _) =
  WithAllConstructors $ mkImportExportName <$> fromGenLocated name
mkImportEntry (GHC.IEThingWith _ name _ constructors _) =
  WithSpecificConstructors
    { name = mkImportExportName <$> fromGenLocated name
    , constructors =
        fmap (fmap mkImportExportName . fromGenLocated) constructors
    }
#else
mkImportEntry (GHC.IEVar _ name) =
  SingleIdentifier $ mkImportExportName <$> fromGenLocated name
mkImportEntry (GHC.IEThingAbs _ name) =
  SingleIdentifier $ mkImportExportName <$> fromGenLocated name
mkImportEntry (GHC.IEThingAll _ name) =
  WithAllConstructors $ mkImportExportName <$> fromGenLocated name
mkImportEntry (GHC.IEThingWith _ name _ constructors) =
  WithSpecificConstructors
    { name = mkImportExportName <$> fromGenLocated name
    , constructors =
        fmap (fmap mkImportExportName . fromGenLocated) constructors
    }
#endif
mkImportEntry _ = undefined

sortVariantsAndExplicitImports ::
     [WithComments ImportEntry] -> [WithComments ImportEntry]
sortVariantsAndExplicitImports = fmap sortVariants . sortExplicitImports

-- | This function sorts variants (e.g., data constructors and class
-- methods) in the given explicit import by their names.
sortVariants :: WithComments ImportEntry -> WithComments ImportEntry
sortVariants = fmap f
  where
    f WithSpecificConstructors {..} =
      WithSpecificConstructors
        {constructors = sortBy (compare `on` getNode) constructors, ..}
    f x = x

-- | This function sorts the given explicit imports by their names.
sortExplicitImports :: [WithComments ImportEntry] -> [WithComments ImportEntry]
sortExplicitImports = sortBy (compareImportEntities `on` getNode)

-- | This function compares two import declarations by their module names.
compareImportEntities :: ImportEntry -> ImportEntry -> Ordering
compareImportEntities = compare `on` getNode . getModuleName

-- | This function returns a 'Just' value with the module name extracted
-- from the import declaration. Otherwise, it returns a 'Nothing'.
getModuleName :: ImportEntry -> WithComments ImportExportName
getModuleName (SingleIdentifier wrapped) = wrapped
getModuleName (WithAllConstructors wrapped) = wrapped
getModuleName (WithSpecificConstructors wrapped _) = wrapped
