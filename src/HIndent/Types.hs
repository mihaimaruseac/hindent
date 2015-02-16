{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | All types.

module HIndent.Types
  (Printer(..)
  ,PrintState(..)
  ,Extender(..)
  ,Style(..)
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
import Data.Default
import Data.Functor.Identity
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Language.Haskell.Exts.Comments
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.SrcLoc

-- | A pretty printing monad.
newtype Printer s a =
  Printer {runPrinter :: StateT (PrintState s) (MaybeT Identity) a}
  deriving (Applicative,Monad,Functor,MonadState (PrintState s),MonadPlus,Alternative)

-- | The state of the pretty printer.
data PrintState s =
  PrintState {psIndentLevel :: !Int64 -- ^ Current indentation level.
             ,psOutput :: !Builder -- ^ The current output.
             ,psNewline :: !Bool -- ^ Just outputted a newline?
             ,psColumn :: !Int64 -- ^ Current column.
             ,psLine :: !Int64 -- ^ Current line number.
             ,psUserState :: !s -- ^ User state.
             ,psExtenders :: ![Extender s] -- ^ Extenders.
             ,psConfig :: !Config -- ^ Config which styles may or may not pay attention to.
             ,psEolComment :: !Bool -- ^ An end of line comment has just been outputted.
             ,psInsideCase :: !Bool -- ^ Whether we're in a case statement, used for Rhs printing.
             ,psParseMode :: !ParseMode -- ^ Mode used to parse the original AST.
             }

instance Eq (PrintState s) where
  PrintState ilevel out newline col line _ _ _ eolc inc pm == PrintState ilevel' out' newline' col' line' _ _ _ eolc' inc' pm' =
    (ilevel,out,newline,col,line,eolc, inc) == (ilevel',out',newline',col',line',eolc', inc')

-- | A printer extender. Takes as argument the user state that the
-- printer was run with, and the current node to print. Use
-- 'prettyNoExt' to fallback to the built-in printer.
data Extender s where
  Extender :: forall s a. (Typeable a) => (a -> Printer s ()) -> Extender s
  CatchAll :: forall s. (forall a. Typeable a => s -> a -> Maybe (Printer s ())) -> Extender s

-- | A printer style.
data Style =
  forall s. Style {styleName :: !Text -- ^ Name of the style, used in the commandline interface.
                  ,styleAuthor :: !Text -- ^ Author of the printer (as opposed to the author of the style).
                  ,styleDescription :: !Text -- ^ Description of the style.
                  ,styleInitialState :: !s -- ^ User state, if needed.
                  ,styleExtenders :: ![Extender s] -- ^ Extenders to the printer.
                  ,styleDefConfig :: !Config -- ^ Default config to use for this style.
                  }

-- | Configurations shared among the different styles. Styles may pay
-- attention to or completely disregard this configuration.
data Config =
  Config {configMaxColumns :: !Int64 -- ^ Maximum columns to fit code into ideally.
         ,configIndentSpaces :: !Int64 -- ^ How many spaces to indent?
         ,configClearEmptyLines :: !Bool  -- ^ Remove spaces on lines that are otherwise empty?
         }

instance Default Config where
  def =
    Config {configMaxColumns = 80
           ,configIndentSpaces = 2
           ,configClearEmptyLines = False}

-- | Default style configuration.
defaultConfig :: Config
defaultConfig = def

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
