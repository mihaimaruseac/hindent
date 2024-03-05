{-# LANGUAGE CPP #-}

module HIndent.Ast.Import
  ( Import
  , mkImport
  , sortByName
  ) where

import           Data.Char
import           Data.Function
import           Data.List
import           Data.Maybe
import qualified GHC.Types.SrcLoc                   as GHC
import           HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
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

-- | The letter type of a 'Char'.
--
-- The order of constructors is important. HIndent sorts explicit imports
-- from ones starting from a capital letter (e.g., data constructors),
-- symbol identifiers, and functions.
data LetterType
  = Capital
  | Symbol
  | Lower
  deriving (Eq, Ord)

-- | This function sorts import declarations and explicit imports in them
-- by their names.
sortImportsByName :: [GHC.LImportDecl GHC.GhcPs] -> [GHC.LImportDecl GHC.GhcPs]
sortImportsByName = fmap sortExplicitImportsInDecl . sortByModuleName

-- | This function sorts import declarations by their module names.
sortByModuleName :: [GHC.LImportDecl GHC.GhcPs] -> [GHC.LImportDecl GHC.GhcPs]
sortByModuleName = sortBy (compare `on` GHC.unLoc . GHC.ideclName . GHC.unLoc)

-- | This function sorts explicit imports in the given import declaration
-- by their names.
sortExplicitImportsInDecl ::
     GHC.LImportDecl GHC.GhcPs -> GHC.LImportDecl GHC.GhcPs
#if MIN_VERSION_ghc_lib_parser(9,6,1)
sortExplicitImportsInDecl (L l d@ImportDecl {ideclImportList = Just (x, imports)}) =
  L l d {ideclImportList = Just (x, sorted)}
  where
    sorted = fmap (fmap sortVariants . sortExplicitImports) imports
#else
sortExplicitImportsInDecl (GHC.L l d@GHC.ImportDecl {GHC.ideclHiding = Just (x, imports)}) =
  GHC.L l d {GHC.ideclHiding = Just (x, sorted)}
  where
    sorted = fmap (fmap sortVariants . sortExplicitImports) imports
#endif
sortExplicitImportsInDecl x = x

-- | This function sorts the given explicit imports by their names.
sortExplicitImports :: [GHC.LIE GHC.GhcPs] -> [GHC.LIE GHC.GhcPs]
sortExplicitImports = sortBy compareImportEntities

-- | This function sorts variants (e.g., data constructors and class
-- methods) in the given explicit import by their names.
sortVariants :: GHC.LIE GHC.GhcPs -> GHC.LIE GHC.GhcPs
sortVariants (GHC.L l (GHC.IEThingWith x x' x'' xs)) =
  GHC.L l $ GHC.IEThingWith x x' x'' (sortWrappedNames xs)
  where
    sortWrappedNames = sortBy (compare `on` showOutputable)
sortVariants x = x

-- | This function compares two import declarations by their module names.
compareImportEntities :: GHC.LIE GHC.GhcPs -> GHC.LIE GHC.GhcPs -> Ordering
compareImportEntities (GHC.L _ a) (GHC.L _ b) =
  fromMaybe LT $ compareIdentifier <$> moduleName a <*> moduleName b

-- | This function returns a 'Just' value with the module name extracted
-- from the import declaration. Otherwise, it returns a 'Nothing'.
moduleName :: GHC.IE GHC.GhcPs -> Maybe String
moduleName (GHC.IEVar _ wrapped)           = Just $ showOutputable wrapped
moduleName (GHC.IEThingAbs _ wrapped)      = Just $ showOutputable wrapped
moduleName (GHC.IEThingAll _ wrapped)      = Just $ showOutputable wrapped
moduleName (GHC.IEThingWith _ wrapped _ _) = Just $ showOutputable wrapped
moduleName _                               = Nothing

-- | This function compares two identifiers in order of capitals, symbols,
-- and lowers.
compareIdentifier :: String -> String -> Ordering
compareIdentifier as@(a:_) bs@(b:_) =
  case compareChar a b of
    EQ -> compareSameIdentifierType as bs
    x  -> x
compareIdentifier _ _ = error "Either identifier is an empty string."

-- | Almost similar to 'compare' but ignores parentheses for symbol
-- identifiers as they are enclosed by parentheses.
compareSameIdentifierType :: String -> String -> Ordering
compareSameIdentifierType "" "" = EQ
compareSameIdentifierType "" _ = LT
compareSameIdentifierType _ "" = GT
compareSameIdentifierType ('(':as) bs = compareSameIdentifierType as bs
compareSameIdentifierType (')':as) bs = compareSameIdentifierType as bs
compareSameIdentifierType as ('(':bs) = compareSameIdentifierType as bs
compareSameIdentifierType as (')':bs) = compareSameIdentifierType as bs
compareSameIdentifierType (a:as) (b:bs) =
  case compare a b of
    EQ -> compareSameIdentifierType as bs
    x  -> x

-- | This function compares two characters by their types (capital, symbol,
-- and lower). If both are the same type, then it compares them by the
-- usual ordering.
compareChar :: Char -> Char -> Ordering
compareChar a b =
  case compare at bt of
    EQ -> compare a b
    x  -> x
  where
    at = charToLetterType a
    bt = charToLetterType b

-- | This function returns a 'LetterType' based on the given character.
charToLetterType :: Char -> LetterType
charToLetterType c
  | isLower c = Lower
  | isUpper c = Capital
  | otherwise = Symbol
