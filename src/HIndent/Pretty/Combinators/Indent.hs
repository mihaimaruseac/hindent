-- | Printer combinators related to indent.
module HIndent.Pretty.Combinators.Indent
  ( indentedBlock
  , indentedWithSpace
  , (|=>)
  , indentedWithFixedLevel
  , prefixed
  , getIndentSpaces
  ) where

import Control.Monad.State
import qualified Data.Text as Text
import HIndent.Config
import HIndent.Pretty.Combinators.String
import HIndent.Printer

-- | This function runs the given printer with an additional indent. The
-- indent has @configIndentSpaces@ spaces.
indentedBlock :: Printer a -> Printer a
indentedBlock p = do
  indentSpaces <- getIndentSpaces
  indentedWithSpace indentSpaces p

-- | This function runs the given printer with an additional indent. The
-- indent has the specified number of spaces.
indentedWithSpace :: Int -> Printer a -> Printer a
indentedWithSpace i p = do
  level <- gets psIndentLevel
  indentedWithFixedLevel (level + i) p

-- | This function runs the first printer, fixes the indent, and then runs
-- the second one.
--
-- For example,
--
-- > string "foo " |=> lined [string "bar", "baz"]
--
-- will print texts as below.
-- foo bar
--     baz
(|=>) :: Printer () -> Printer a -> Printer a
hd |=> p = do
  hd
  col <- gets psColumn
  indentedWithFixedLevel col p

infixl 1 |=>
-- | This function runs the given printer with the passed indent level.
indentedWithFixedLevel :: Int -> Printer a -> Printer a
indentedWithFixedLevel i p = do
  l <- gets psIndentLevel
  modify (\s -> s {psIndentLevel = i})
  m <- p
  modify (\s -> s {psIndentLevel = l})
  return m

-- | Prints the text passed as the first argument before the current
-- position and then the second argument.
prefixed :: Text.Text -> Printer () -> Printer ()
prefixed s p = do
  indentedWithSpace (-(Text.length s)) $ string s
  p

-- | This function returns the current indent level.
getIndentSpaces :: Printer Int
getIndentSpaces = gets (configIndentSpaces . psConfig)
