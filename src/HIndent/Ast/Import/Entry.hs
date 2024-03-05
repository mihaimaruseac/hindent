{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Import.Entry
  ( ImportEntry
  , mkImportEntry
  , sortVariants
  , sortExplicitImports
  ) where

import           Data.Char
import           Data.Function
import           Data.List
import qualified GHC.Hs                      as GHC
import           HIndent.Ast.NodeComments
import           HIndent.Ast.WithComments
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments

data ImportEntry
  = SingleIdentifier String
  | WithSpecificConstructors
      { name         :: String
      , constructors :: [String]
      }
  | WithAllConstructors String

instance CommentExtraction ImportEntry where
  nodeComments _ = NodeComments [] [] []

instance Pretty ImportEntry where
  pretty' (SingleIdentifier wrapped) = string wrapped
  pretty' (WithAllConstructors wrapped) = string wrapped >> string "(..)"
  pretty' WithSpecificConstructors {..} =
    string name >> hTuple (fmap string constructors)

mkImportEntry :: GHC.IE GHC.GhcPs -> ImportEntry
mkImportEntry (GHC.IEVar _ name) = SingleIdentifier $ showOutputable name
mkImportEntry (GHC.IEThingAbs _ name) = SingleIdentifier $ showOutputable name
mkImportEntry (GHC.IEThingAll _ name) =
  WithAllConstructors $ showOutputable name
mkImportEntry (GHC.IEThingWith _ name _ xs) =
  WithSpecificConstructors
    {name = showOutputable name, constructors = fmap showOutputable xs}
mkImportEntry _ = undefined

-- | This function sorts variants (e.g., data constructors and class
-- methods) in the given explicit import by their names.
sortVariants :: WithComments ImportEntry -> WithComments ImportEntry
sortVariants = fmap f
  where
    f WithSpecificConstructors {..} =
      WithSpecificConstructors {constructors = sort constructors, ..}
    f x = x

-- | This function sorts the given explicit imports by their names.
sortExplicitImports :: [WithComments ImportEntry] -> [WithComments ImportEntry]
sortExplicitImports = sortBy (compareImportEntities `on` getNode)

-- | This function compares two import declarations by their module names.
compareImportEntities :: ImportEntry -> ImportEntry -> Ordering
compareImportEntities = compareIdentifier `on` getModuleName

-- | This function returns a 'Just' value with the module name extracted
-- from the import declaration. Otherwise, it returns a 'Nothing'.
getModuleName :: ImportEntry -> String
getModuleName (SingleIdentifier wrapped)           = wrapped
getModuleName (WithAllConstructors wrapped)        = wrapped
getModuleName (WithSpecificConstructors wrapped _) = wrapped

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
