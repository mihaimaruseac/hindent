{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | All types.

module HIndent.Types
  (Printer(..)
  ,PrintState(..)
  ,Extender(..)
  ,Style(..)
  ,Config(..)
  ,NodeInfo(..)
  ,ComInfo(..))
  where

import Control.Monad.State (MonadState(..),State)
import Data.Data
import Data.Default
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Language.Haskell.Exts.Comments
import Language.Haskell.Exts.SrcLoc

-- | A pretty printing monad.
newtype Printer a = Printer { runPrinter :: State PrintState a }
  deriving (Monad,Functor,MonadState PrintState)

-- | The state of the pretty printer.
data PrintState =
  forall s. PrintState {psIndentLevel :: !Int64 -- ^ Current indentation level.
                       ,psOutput :: !Builder -- ^ The current output.
                       ,psNewline :: !Bool -- ^ Just outputted a newline?
                       ,psColumn :: !Int64 -- ^ Current column.
                       ,psLine :: !Int64 -- ^ Current line number.
                       ,psUserState :: !s -- ^ User state.
                       ,psExtenders :: ![Extender s] -- ^ Extenders.
                       ,psConfig :: !Config -- ^ Config which styles may or may not pay attention to.
                       ,psEolComment :: !Bool -- ^ An end of line comment has just been outputted.
                        }

instance Eq PrintState where
  PrintState ilevel out newline col line _ _ _ eolc == PrintState ilevel' out' newline' col' line' _ _ _ eolc' =
    (ilevel,out,newline,col,line,eolc) == (ilevel',out',newline',col',line',eolc')

-- | A printer extender. Takes as argument the user state that the
-- printer was run with, and the current node to print. Use
-- 'prettyInternal' to fallback to the built-in printer.
data Extender s =
  forall a. (Typeable a) => Extender (s -> a -> Printer ())

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
          }

instance Default Config where
  def =
    Config {configMaxColumns = 80
           ,configIndentSpaces = 2}

-- | Information for each node in the AST.
data NodeInfo =
  NodeInfo {nodeInfoSpan :: !SrcSpanInfo -- ^ Location info from the parser.
           ,nodeInfoComments :: ![ComInfo] -- ^ Comments which follow this node.
            }
  deriving (Typeable,Show,Data)

-- | Comment with some more info.
data ComInfo =
  ComInfo {comInfoComment :: !Comment
          ,comInfoOwnLine :: !Bool}
  deriving (Show,Typeable,Data)
