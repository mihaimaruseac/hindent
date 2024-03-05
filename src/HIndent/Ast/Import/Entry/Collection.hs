{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module HIndent.Ast.Import.Entry.Collection
  ( ImportEntryCollection
  , mkImportEntryCollection
  , sortEntriesByName
  ) where

import           Control.Monad
import           Data.Char
import           Data.Function
import           Data.List
import           Data.Maybe
import qualified GHC.Hs                               as GHC
import           HIndent.Ast.Import.ImportingOrHiding
import           HIndent.Ast.NodeComments
import           HIndent.Ast.WithComments
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments

data ImportEntryCollection = ImportEntryCollection
  { entries :: [WithComments (GHC.IE GHC.GhcPs)]
  , kind    :: ImportingOrHiding
  }

instance CommentExtraction ImportEntryCollection where
  nodeComments ImportEntryCollection {} = NodeComments [] [] []

instance Pretty ImportEntryCollection where
  pretty' ImportEntryCollection {..} = do
    when (kind == Hiding) $ string " hiding"
    (space >> hTuple (fmap pretty entries)) <-|>
      (newline >> indentedBlock (vTuple $ fmap pretty entries))

mkImportEntryCollection ::
     GHC.ImportDecl GHC.GhcPs -> Maybe (WithComments ImportEntryCollection)
mkImportEntryCollection GHC.ImportDecl {..} =
  case ideclHiding of
    Nothing -> Nothing
    Just (False, xs) ->
      Just $
      fmap (\entries -> ImportEntryCollection {kind = Importing, ..}) $
      fromGenLocated $ fmap (fmap fromGenLocated) xs
    Just (True, xs) ->
      Just $
      fmap (\entries -> ImportEntryCollection {kind = Hiding, ..}) $
      fromGenLocated $ fmap (fmap fromGenLocated) xs

sortEntriesByName :: ImportEntryCollection -> ImportEntryCollection
sortEntriesByName ImportEntryCollection {..} =
  ImportEntryCollection
    {entries = sortVariants <$> sortExplicitImports entries, ..}

-- | This function sorts the given explicit imports by their names.
sortExplicitImports ::
     [WithComments (GHC.IE GHC.GhcPs)] -> [WithComments (GHC.IE GHC.GhcPs)]
sortExplicitImports = sortBy (compareImportEntities `on` getNode)

-- | This function sorts variants (e.g., data constructors and class
-- methods) in the given explicit import by their names.
sortVariants ::
     WithComments (GHC.IE GHC.GhcPs) -> WithComments (GHC.IE GHC.GhcPs)
sortVariants = fmap f
  where
    f (GHC.IEThingWith x x' x'' xs) =
      GHC.IEThingWith x x' x'' (sortWrappedNames xs)
      where
        sortWrappedNames = sortBy (compare `on` showOutputable)
    f x = x

-- | This function compares two import declarations by their module names.
compareImportEntities :: GHC.IE GHC.GhcPs -> GHC.IE GHC.GhcPs -> Ordering
compareImportEntities a b =
  fromMaybe LT $ compareIdentifier <$> getModuleName a <*> getModuleName b

-- | This function returns a 'Just' value with the module name extracted
-- from the import declaration. Otherwise, it returns a 'Nothing'.
getModuleName :: GHC.IE GHC.GhcPs -> Maybe String
getModuleName (GHC.IEVar _ wrapped)           = Just $ showOutputable wrapped
getModuleName (GHC.IEThingAbs _ wrapped)      = Just $ showOutputable wrapped
getModuleName (GHC.IEThingAll _ wrapped)      = Just $ showOutputable wrapped
getModuleName (GHC.IEThingWith _ wrapped _ _) = Just $ showOutputable wrapped
getModuleName _                               = Nothing

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
