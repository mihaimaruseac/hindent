-- | Getters to fetch current status and printer information.
module HIndent.Pretty.Combinators.Getter
  ( startingColumn
  , printerLength
  ) where

import           Control.Monad.RWS                 hiding (state)
import           Data.Int
import           HIndent.Pretty.Combinators.String
import           HIndent.Types

-- | Returns the column from which a new string is printed. It may be
-- different from 'psColumn' immediately after printing a comment.
startingColumn :: Printer Int64
startingColumn = do
  before <- get
  string ""
  after <- get
  put before
  return $ psColumn after

-- Returns how many characters the printer moved the cursor horizontally.
-- The returned value maybe negative if the printer prints multiple lines
-- and the column of the last position is less than before.
printerLength :: Printer a -> Printer Int64
printerLength p = do
  before <- get
  _ <- p
  after <- get
  put before
  pure $ psColumn after - psColumn before
