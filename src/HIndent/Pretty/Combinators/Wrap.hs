-- | Printer operators for wrapping texts with a prefix and a suffix.
module HIndent.Pretty.Combinators.Wrap
  ( parens
  , parensIfSymbol
  , bananaBrackets
  , braces
  , doubleQuotes
  , brackets
  , typedBrackets
  , backticks
  , backticksIfNotSymbol
  , wrapWithBars
  , promotedListBrackets
  , promotedTupleParens
  , unboxedSums
  ) where

import           GHC.Types.Name
import           HIndent.Pretty.Combinators.Indent
import           HIndent.Pretty.Combinators.String
import           HIndent.Types

-- | This function wraps the printer with parentheses.
parens :: Printer a -> Printer a
parens = wrap "(" ")"

-- | This function wraps the printer with parentheses if the identifier
-- contains only symbols.
parensIfSymbol :: OccName -> Printer a -> Printer a
parensIfSymbol name
  | isSymOcc name = parens
  | otherwise = id

-- | Wraps with "(|" and "|)"
bananaBrackets :: Printer a -> Printer a
bananaBrackets = wrap "(|" "|)"

-- | This function wraps the printer with braces.
braces :: Printer a -> Printer a
braces = wrap "{" "}"

-- | This function wraps the printer with brackets.
brackets :: Printer a -> Printer a
brackets = wrap "[" "]"

-- | Wraps with @[|| @ and @ ||]@.
typedBrackets :: Printer a -> Printer a
typedBrackets = wrap "[|| " " ||]"

-- | Wraps with double quotes.
doubleQuotes :: Printer a -> Printer a
doubleQuotes = wrap "\"" "\""

-- | This function wraps the printer with backticks.
backticks :: Printer a -> Printer a
backticks = wrap "`" "`"

-- | This function wraps the printer with backticks if the identifier
-- contains at least one non-symbol character.
backticksIfNotSymbol :: OccName -> Printer a -> Printer a
backticksIfNotSymbol name
  | isSymOcc name = id
  | otherwise = backticks

-- | This function wraps the printer with bars (|).
wrapWithBars :: Printer a -> Printer a
wrapWithBars = wrap "|" "|"

-- | This function wraps the printer with @'[ @ and @]@ for a promoted
-- list.
promotedListBrackets :: Printer a -> Printer a
promotedListBrackets = wrap "'[ " "]"

-- | This function wraps the printer with @'( @ and @)@ for a promoted
-- tuple.
promotedTupleParens :: Printer a -> Printer a
promotedTupleParens = wrap "'( " ")"

-- | Wraps with @(# @ and @ #)@.
unboxedSums :: Printer a -> Printer a
unboxedSums = wrap "(# " " #)"

-- | This function wraps the printer with the prefix and the suffix.
wrap :: String -> String -> Printer a -> Printer a
wrap open close p = string open |=> p <* string close
