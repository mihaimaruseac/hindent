{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | All types.

module HIndent.Types
  (Printer(..)
  ,PrintState(..)
  ,Pretty(..))
  where

import Control.Monad.State
import Data.Int
import Data.Text.Lazy.Builder (Builder)

-- | Pretty printing class.
class Pretty a where
  pretty :: a -> Printer ()

-- | A pretty printing monad.
newtype Printer a = Printer { runPrinter :: State PrintState a }
  deriving (Monad,Functor,MonadState PrintState)

-- | The state of the pretty printer.
data PrintState = PrintState
  { psIndentLevel :: !Int64   -- ^ Current indentation level.
  , psOutput      :: !Builder -- ^ The current output.
  , psNewline     :: !Bool    -- ^ Just outputted a newline?
  , psColumn      :: !Int64   -- ^ Current column.
  , psLine        :: !Int64   -- ^ Current line number.
  } deriving (Show,Eq)
