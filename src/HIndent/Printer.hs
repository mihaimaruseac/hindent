{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

-- | Printer types.
module HIndent.Printer
  ( Printer(..)
  , PrintState(..)
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict (MonadState(..), StateT)
import Control.Monad.Trans.Maybe
import Data.ByteString.Builder
import Data.Functor.Identity
import Data.Int (Int64)
import HIndent.Config

-- | A pretty printing monad.
newtype Printer a = Printer
  { runPrinter :: StateT PrintState (MaybeT Identity) a
  } deriving ( Applicative
             , Monad
             , Functor
             , MonadState PrintState
             , MonadPlus
             , Alternative
             )

-- | The state of the pretty printer.
data PrintState = PrintState
  { psIndentLevel :: !Int64
    -- ^ Current indentation level, i.e. every time there's a
    -- new-line, output this many spaces.
  , psOutput :: !Builder
    -- ^ The current output bytestring builder.
  , psNewline :: !Bool
    -- ^ Just outputted a newline?
  , psColumn :: !Int64
    -- ^ Current column.
  , psLine :: !Int64
    -- ^ Current line number.
  , psConfig :: !Config
    -- ^ Configuration of max colums and indentation style.
  , psFitOnOneLine :: !Bool
    -- ^ Bail out if we need to print beyond the current line or
    -- the maximum column.
  , psEolComment :: !Bool
  }
