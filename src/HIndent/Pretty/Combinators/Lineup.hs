-- | Printer combinators for lining up multiple elements.
module HIndent.Pretty.Combinators.Lineup
  ( -- * Tuples
    hvTuple
  , hvTuple'
  , hTuple
  , hFillingTuple
  , vTuple
  , vTuple'
  , hPromotedTuple
  , -- * Unboxed tuples
    hvUnboxedTuple'
  , hUnboxedTuple
  , -- * Unboxed sums
    hvUnboxedSum'
  , -- * Records
    hvFields
  , hFields
  , vFields
  , vFields'
  , -- * Lists
    hList
  , vList
  , hvPromotedList
  , -- * Bars
    hvBarSep
  , hBarSep
  , vBarSep
  , -- * Commas
    hvCommaSep
  , hCommaSep
  , vCommaSep
  , -- * Others
    spaced
  , lined
  , blanklined
  , hDotSep
  , spacePrefixed
  , newlinePrefixed
  , prefixedLined
  , inter
  ) where

import Control.Monad
import Data.Foldable (toList)
import Data.List (intersperse)
import HIndent.Pretty.Combinators.Indent
import HIndent.Pretty.Combinators.String
import HIndent.Pretty.Combinators.Switch
import HIndent.Pretty.Combinators.Wrap
import HIndent.Printer

-- | Applies 'hTuple' if the result fits in a line or 'vTuple' otherwise.
hvTuple :: Foldable f => f (Printer ()) -> Printer ()
hvTuple = (<-|>) <$> hTuple <*> vTuple

-- | Applies 'hTuple'' if the result fits in a line or 'vTuple'' otherwise.
hvTuple' :: Foldable f => f (Printer ()) -> Printer ()
hvTuple' = (<-|>) <$> hTuple <*> vTuple'

-- | Runs printers to construct a tuple in a line.
hTuple :: Foldable f => f (Printer ()) -> Printer ()
hTuple = parens . hCommaSep

-- | Runs printers to construct a tuple in a line, but inserts newlines if
-- the result doesn't fit in a line.
--
-- The difference between this function and 'vTuple' is that the number of elements
-- in a row in this function is not limited to 1 while the number of elements in
-- a row in 'vTuple' is limited to 1.
hFillingTuple :: Foldable f => f (Printer ()) -> Printer ()
hFillingTuple = parens . inter (comma >> (space <-|> newline))

-- | Runs printers to construct a tuple where elements are aligned
-- vertically.
vTuple :: Foldable f => f (Printer ()) -> Printer ()
vTuple = vCommaSepWrapped ("(", ")")

-- | Similar to 'vTuple', but the closing parenthesis is in the last
-- element.
vTuple' :: Foldable f => f (Printer ()) -> Printer ()
vTuple' = vCommaSepWrapped' ("(", ")")

-- | Runs printers to construct a promoted tuple in a line.
hPromotedTuple :: Foldable f => f (Printer ()) -> Printer ()
hPromotedTuple = promotedTupleParens . hCommaSep

-- | Runs printers to construct an unboxed tuple. The elements are aligned
-- either in a line or vertically.
hvUnboxedTuple' :: Foldable f => f (Printer ()) -> Printer ()
hvUnboxedTuple' = (<-|>) <$> hUnboxedTuple <*> vUnboxedTuple'

-- | Runs printers to construct an unboxed tuple in a line.
hUnboxedTuple :: Foldable f => f (Printer ()) -> Printer ()
hUnboxedTuple = unboxedParens . hCommaSep

-- | Runs printers to construct an unboxed tuple where the elements are
-- aligned vertically.
vUnboxedTuple' :: Foldable f => f (Printer ()) -> Printer ()
vUnboxedTuple' = vCommaSepWrapped' ("(#", " #)")

-- | Runs printers to construct an unboxed sum. The elements are aligned
-- either in a line or vertically.
--
-- The enclosing parenthesis will be printed on the same line as the last
-- element.
hvUnboxedSum' :: Foldable f => f (Printer ()) -> Printer ()
hvUnboxedSum' = (<-|>) <$> hUnboxedSum <*> vUnboxedSum'

-- | Runs printers to construct an unboxed sum in a line.
hUnboxedSum :: Foldable f => f (Printer ()) -> Printer ()
hUnboxedSum = unboxedParens . hBarSep

-- | Runs printers to construct an unboxed sum where the elements are
-- aligned vertically.
--
-- The enclosing parenthesis will be printed on the same line as the last
-- element.
vUnboxedSum' :: Foldable f => f (Printer ()) -> Printer ()
vUnboxedSum' = vWrappedLineup' '|' ("(#", " #)")

-- | Applies 'hFields' if the result fits in a line or 'vFields' otherwise.
hvFields :: Foldable f => f (Printer ()) -> Printer ()
hvFields = (<-|>) <$> hFields <*> vFields

-- | Runs printers to construct a record in a line.
hFields :: Foldable f => f (Printer ()) -> Printer ()
hFields = braces . hCommaSep

-- | Runs printers to construct a record where elements are aligned
-- vertically.
vFields :: Foldable f => f (Printer ()) -> Printer ()
vFields = vCommaSepWrapped ("{", "}")

-- | Similar to 'vFields', but the closing brace is in the same line as the
-- last element.
vFields' :: Foldable f => f (Printer ()) -> Printer ()
vFields' = vCommaSepWrapped' ("{", "}")

-- | Runs printers to construct a list in a line.
hList :: Foldable f => f (Printer ()) -> Printer ()
hList = brackets . hCommaSep

-- | Runs printers to construct a list where elements are aligned
-- vertically.
vList :: Foldable f => f (Printer ()) -> Printer ()
vList = vCommaSepWrapped ("[", "]")

-- | Runs printers to construct a promoted list where elements are aligned
-- in a line or vertically.
hvPromotedList :: Foldable f => f (Printer ()) -> Printer ()
hvPromotedList = (<-|>) <$> hPromotedList <*> vPromotedList

-- | Runs printers to construct a promoted list in a line.
hPromotedList :: Foldable f => f (Printer ()) -> Printer ()
hPromotedList = promotedListBrackets . hCommaSep

-- | Runs printers to construct a promoted list where elements are aligned
-- vertically.
vPromotedList :: Foldable f => f (Printer ()) -> Printer ()
vPromotedList = vCommaSepWrapped ("'[", " ]")

-- | Runs printers in a line with a space as the separator.
spaced :: Foldable f => f (Printer ()) -> Printer ()
spaced = inter space

-- | Runs printers line by line.
lined :: Foldable f => f (Printer ()) -> Printer ()
lined = inter newline

-- | Runs printers with a blank line as the separator.
blanklined :: Foldable f => f (Printer ()) -> Printer ()
blanklined = inter blankline

-- | Applies 'hBarSep' if the result fits in a line or 'vBarSep' otherwise.
hvBarSep :: Foldable f => f (Printer ()) -> Printer ()
hvBarSep = (<-|>) <$> hBarSep <*> vBarSep

-- | Runs printers in a line with a bar as the separator.
hBarSep :: Foldable f => f (Printer ()) -> Printer ()
hBarSep = inter (string " | ")

-- | Runs printers where each line except the first one has @| @ as
-- a prefix.
vBarSep :: Foldable f => f (Printer ()) -> Printer ()
vBarSep = prefixedLined "| "

-- | Applies 'hCommaSep' if the result fits in a line or 'vCommaSep'
-- otherwise.
hvCommaSep :: Foldable f => f (Printer ()) -> Printer ()
hvCommaSep = (<-|>) <$> hCommaSep <*> vCommaSep

-- | Runs printers in a line with a comma as the separator.
hCommaSep :: Foldable f => f (Printer ()) -> Printer ()
hCommaSep = inter (string ", ")

-- | Runs printers with each line except the first one has @, @ as
-- a prefix.
vCommaSep :: Foldable f => f (Printer ()) -> Printer ()
vCommaSep = prefixedLined ", "

-- | Prints elements separated by comma  in vertical with the given prefix
-- and suffix.
vCommaSepWrapped ::
     Foldable f => (String, String) -> f (Printer ()) -> Printer ()
vCommaSepWrapped = vWrappedLineup ','

-- | Similar to 'vCommaSepWrapped' but the suffix is in the same line as the last
-- element.
vCommaSepWrapped' ::
     Foldable f => (String, String) -> f (Printer ()) -> Printer ()
vCommaSepWrapped' = vWrappedLineup' ','

-- | Runs printers with a dot as the separator.
hDotSep :: Foldable f => f (Printer ()) -> Printer ()
hDotSep = inter (string ".")

-- | Prints each element after a space like.
spacePrefixed :: Foldable f => f (Printer ()) -> Printer ()
spacePrefixed = mapM_ (space >>)

-- | Prints each element after a new line.
newlinePrefixed :: Foldable f => f (Printer ()) -> Printer ()
newlinePrefixed = mapM_ (newline >>)

-- | Runs printers with a prefix. The prefix is printed before the indent.
prefixedLined :: Foldable f => String -> f (Printer ()) -> Printer ()
prefixedLined pref printers =
  case toList printers of
    [] -> return ()
    x:xs -> do
      x
      forM_ xs $ \p -> do
        newline
        prefixed pref p

-- | Prints elements in vertical with the given prefix, suffix, and
-- separator.
vWrappedLineup ::
     Foldable f => Char -> (String, String) -> f (Printer ()) -> Printer ()
vWrappedLineup sep (prefix, suffix) ps =
  string prefix
    >> space |=> do
         prefixedLined [sep, ' '] ps
         newline
         indentedWithSpace (-(fromIntegral (length prefix) + 1)) $ string suffix

-- | Similar to 'vWrappedLineup' but the suffix is in the same line as the
-- last element.
vWrappedLineup' ::
     Foldable f => Char -> (String, String) -> f (Printer ()) -> Printer ()
vWrappedLineup' sep (prefix, suffix) printers =
  case toList printers of
    [x] -> spaced [string prefix, x, string suffix]
    ps ->
      string prefix
        >> space |=> do
             prefixedLined [sep, ' '] ps
             string suffix

-- Inserts the first printer between each element of the list passed as the
-- second argument and runs them.
inter :: Foldable f => Printer () -> f (Printer ()) -> Printer ()
inter separator = sequence_ . intersperse separator . toList
