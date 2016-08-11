{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

-- | All types.

module HIndent.Types
  (Printer(..)
  ,PrintState(..)
  ,Config(..)
  ,defaultConfig
  ,NodeInfo(..)
  ,ComInfo(..)
  ,ComInfoLocation(..)
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict (MonadState(..),StateT)
import Control.Monad.Trans.Maybe
import Data.Data
import Data.Functor.Identity
import Data.Int (Int64)
import Data.ByteString.Builder
import Language.Haskell.Exts.Comments
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.SrcLoc

-- | A pretty printing monad.
newtype Printer a =
  Printer {runPrinter :: StateT PrintState (MaybeT Identity) a}
  deriving (Applicative,Monad,Functor,MonadState PrintState,MonadPlus,Alternative)

-- | The state of the pretty printer.
data PrintState =
  PrintState {psIndentLevel :: !Int64 -- ^ Current indentation level.
             ,psOutput :: !Builder -- ^ The current output.
             ,psNewline :: !Bool -- ^ Just outputted a newline?
             ,psColumn :: !Int64 -- ^ Current column.
             ,psLine :: !Int64 -- ^ Current line number.
             ,psConfig :: !Config -- ^ Config which styles may or may not pay attention to.
             ,psEolComment :: !Bool -- ^ An end of line comment has just been outputted.
             ,psInsideCase :: !Bool -- ^ Whether we're in a case statement, used for Rhs printing.
             ,psParseMode :: !ParseMode -- ^ Mode used to parse the original AST.
             }

-- | Configurations shared among the different styles. Styles may pay
-- attention to or completely disregard this configuration.
data Config =
  Config {configMaxColumns :: !Int64 -- ^ Maximum columns to fit code into ideally.
         ,configIndentSpaces :: !Int64 -- ^ How many spaces to indent?
         ,configClearEmptyLines :: !Bool  -- ^ Remove spaces on lines that are otherwise empty?
         }

-- | Default style configuration.
defaultConfig :: Config
defaultConfig =
  Config {configMaxColumns = 80
         ,configIndentSpaces = 2
         ,configClearEmptyLines = False}

-- | Information for each node in the AST.
data NodeInfo =
  NodeInfo {nodeInfoSpan :: !SrcSpanInfo -- ^ Location info from the parser.
           ,nodeInfoComments :: ![ComInfo] -- ^ Comments which are attached to this node.
           }
  deriving (Typeable,Show,Data)

-- | Comment relative locations.
data ComInfoLocation = Before | After
  deriving (Show,Typeable,Data,Eq)

-- | Comment with some more info.
data ComInfo =
  ComInfo {comInfoComment :: !Comment                -- ^ The normal comment type.
          ,comInfoLocation :: !(Maybe ComInfoLocation) -- ^ Where the comment lies relative to the node.
          }
  deriving (Show,Typeable,Data)
