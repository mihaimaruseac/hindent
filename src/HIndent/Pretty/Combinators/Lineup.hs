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
import Data.List
import HIndent.Pretty.Combinators.Indent
import HIndent.Pretty.Combinators.String
import HIndent.Pretty.Combinators.Switch
import HIndent.Pretty.Combinators.Wrap
import HIndent.Printer

-- | Applies 'hTuple' if the result fits in a line or 'vTuple' otherwise.
hvTuple :: [Printer ()] -> Printer ()
hvTuple = (<-|>) <$> hTuple <*> vTuple

-- | Applies 'hTuple'' if the result fits in a line or 'vTuple'' otherwise.
hvTuple' :: [Printer ()] -> Printer ()
hvTuple' = (<-|>) <$> hTuple <*> vTuple'

-- | Runs printers to construct a tuple in a line.
hTuple :: [Printer ()] -> Printer ()
hTuple = parens . hCommaSep

-- | Runs printers to construct a tuple in a line, but inserts newlines if
-- the result doesn't fit in a line.
--
-- The difference between this function and 'vTuple' is that the number of elements
-- in a row in this function is not limited to 1 while the number of elements in
-- a row in 'vTuple' is limited to 1.
hFillingTuple :: [Printer ()] -> Printer ()
hFillingTuple = parens . inter (comma >> (space <-|> newline))

-- | Runs printers to construct a tuple where elements are aligned
-- vertically.
vTuple :: [Printer ()] -> Printer ()
vTuple = vCommaSepWrapped ("(", ")")

-- | Similar to 'vTuple', but the closing parenthesis is in the last
-- element.
vTuple' :: [Printer ()] -> Printer ()
vTuple' = vCommaSepWrapped' ("(", ")")

-- | Runs printers to construct a promoted tuple in a line.
hPromotedTuple :: [Printer ()] -> Printer ()
hPromotedTuple = promotedTupleParens . hCommaSep

-- | Runs printers to construct an unboxed tuple. The elements are aligned
-- either in a line or vertically.
hvUnboxedTuple' :: [Printer ()] -> Printer ()
hvUnboxedTuple' = (<-|>) <$> hUnboxedTuple <*> vUnboxedTuple'

-- | Runs printers to construct an unboxed tuple in a line.
hUnboxedTuple :: [Printer ()] -> Printer ()
hUnboxedTuple = unboxedParens . hCommaSep

-- | Runs printers to construct an unboxed tuple where the elements are
-- aligned vertically.
vUnboxedTuple' :: [Printer ()] -> Printer ()
vUnboxedTuple' = vCommaSepWrapped' ("(#", " #)")

-- | Runs printers to construct an unboxed sum. The elements are aligned
-- either in a line or vertically.
--
-- The enclosing parenthesis will be printed on the same line as the last
-- element.
hvUnboxedSum' :: [Printer ()] -> Printer ()
hvUnboxedSum' = (<-|>) <$> hUnboxedSum <*> vUnboxedSum'

-- | Runs printers to construct an unboxed sum in a line.
hUnboxedSum :: [Printer ()] -> Printer ()
hUnboxedSum = unboxedParens . hBarSep

-- | Runs printers to construct an unboxed sum where the elements are
-- aligned vertically.
--
-- The enclosing parenthesis will be printed on the same line as the last
-- element.
vUnboxedSum' :: [Printer ()] -> Printer ()
vUnboxedSum' = vWrappedLineup' '|' ("(#", " #)")

-- | Applies 'hFields' if the result fits in a line or 'vFields' otherwise.
hvFields :: [Printer ()] -> Printer ()
hvFields = (<-|>) <$> hFields <*> vFields

-- | Runs printers to construct a record in a line.
hFields :: [Printer ()] -> Printer ()
hFields = braces . hCommaSep

-- | Runs printers to construct a record where elements are aligned
-- vertically.
vFields :: [Printer ()] -> Printer ()
vFields = vCommaSepWrapped ("{", "}")

-- | Similar to 'vFields', but the closing brace is in the same line as the
-- last element.
vFields' :: [Printer ()] -> Printer ()
vFields' = vCommaSepWrapped' ("{", "}")

-- | Runs printers to construct a list in a line.
hList :: [Printer ()] -> Printer ()
hList = brackets . hCommaSep

-- | Runs printers to construct a list where elements are aligned
-- vertically.
vList :: [Printer ()] -> Printer ()
vList = vCommaSepWrapped ("[", "]")

-- | Runs printers to construct a promoted list where elements are aligned
-- in a line or vertically.
hvPromotedList :: [Printer ()] -> Printer ()
hvPromotedList = (<-|>) <$> hPromotedList <*> vPromotedList

-- | Runs printers to construct a promoted list in a line.
hPromotedList :: [Printer ()] -> Printer ()
hPromotedList = promotedListBrackets . hCommaSep

-- | Runs printers to construct a promoted list where elements are aligned
-- vertically.
vPromotedList :: [Printer ()] -> Printer ()
vPromotedList = vCommaSepWrapped ("'[", " ]")

-- | Runs printers in a line with a space as the separator.
spaced :: [Printer ()] -> Printer ()
spaced = inter space

-- | Runs printers line by line.
lined :: [Printer ()] -> Printer ()
lined = inter newline

-- | Runs printers with a blank line as the separator.
blanklined :: [Printer ()] -> Printer ()
blanklined = inter blankline

-- | Applies 'hBarSep' if the result fits in a line or 'vBarSep' otherwise.
hvBarSep :: [Printer ()] -> Printer ()
hvBarSep = (<-|>) <$> hBarSep <*> vBarSep

-- | Runs printers in a line with a bar as the separator.
hBarSep :: [Printer ()] -> Printer ()
hBarSep = inter (string " | ")

-- | Runs printers where each line except the first one has @| @ as
-- a prefix.
vBarSep :: [Printer ()] -> Printer ()
vBarSep = prefixedLined "| "

-- | Applies 'hCommaSep' if the result fits in a line or 'vCommaSep'
-- otherwise.
hvCommaSep :: [Printer ()] -> Printer ()
hvCommaSep = (<-|>) <$> hCommaSep <*> vCommaSep

-- | Runs printers in a line with a comma as the separator.
hCommaSep :: [Printer ()] -> Printer ()
hCommaSep = inter (string ", ")

-- | Runs printers with each line except the first one has @, @ as
-- a prefix.
vCommaSep :: [Printer ()] -> Printer ()
vCommaSep = prefixedLined ", "

-- | Prints elements separated by comma  in vertical with the given prefix
-- and suffix.
vCommaSepWrapped :: (String, String) -> [Printer ()] -> Printer ()
vCommaSepWrapped = vWrappedLineup ','

-- | Similar to 'vCommaSepWrapped' but the suffix is in the same line as the last
-- element.
vCommaSepWrapped' :: (String, String) -> [Printer ()] -> Printer ()
vCommaSepWrapped' = vWrappedLineup' ','

-- | Runs printers with a dot as the separator.
hDotSep :: [Printer ()] -> Printer ()
hDotSep = inter (string ".")

-- | Prints each element after a space like.
spacePrefixed :: [Printer ()] -> Printer ()
spacePrefixed = mapM_ (space >>)

-- | Prints each element after a new line.
newlinePrefixed :: [Printer ()] -> Printer ()
newlinePrefixed = mapM_ (newline >>)

-- | Runs printers with a prefix. The prefix is printed before the indent.
prefixedLined :: String -> [Printer ()] -> Printer ()
prefixedLined _ [] = return ()
prefixedLined pref (x:xs) = do
  x
  forM_ xs $ \p -> do
    newline
    prefixed pref p

-- | Prints elements in vertical with the given prefix, suffix, and
-- separator.
vWrappedLineup :: Char -> (String, String) -> [Printer ()] -> Printer ()
vWrappedLineup sep (prefix, suffix) ps =
  string prefix
    >> space |=> do
         prefixedLined [sep, ' '] ps
         newline
         indentedWithSpace (-(fromIntegral (length prefix) + 1)) $ string suffix

-- | Similar to 'vWrappedLineup' but the suffix is in the same line as the
-- last element.
vWrappedLineup' :: Char -> (String, String) -> [Printer ()] -> Printer ()
vWrappedLineup' _ (prefix, suffix) [x] =
  spaced [string prefix, x, string suffix]
vWrappedLineup' sep (prefix, suffix) ps =
  string prefix
    >> space |=> do
         prefixedLined [sep, ' '] ps
         string suffix

-- Inserts the first printer between each element of the list passed as the
-- second argument and runs them.
inter :: Printer () -> [Printer ()] -> Printer ()
inter separator = sequence_ . intersperse separator
