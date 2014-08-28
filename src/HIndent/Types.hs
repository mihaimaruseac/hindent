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
  ,Pretty(..))
  where

import Control.Monad.State (MonadState(..),State)
import Data.Default
import Data.Int (Int64)
import Data.Maybe (listToMaybe,mapMaybe)
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Typeable (Typeable,cast)

-- | Pretty printing class.
class (Typeable a) => Pretty a where
  pretty :: a -> Printer ()
  pretty a =
    do st <- get
       case st of
         PrintState{psExtenders = es,psUserState = s} ->
           case listToMaybe (mapMaybe (makePrinter s) es) of
             Just m -> m
             Nothing -> prettyInternal a
    where makePrinter s (Extender f) =
            case cast a of
              Just v -> Just (f s v)
              Nothing -> Nothing
  prettyInternal :: a -> Printer ()

-- | A pretty printing monad.
newtype Printer a = Printer { runPrinter :: State PrintState a }
  deriving (Monad,Functor,MonadState PrintState)

-- | The state of the pretty printer.
data PrintState = forall s. PrintState
  { psIndentLevel :: !Int64        -- ^ Current indentation level.
  , psOutput      :: !Builder      -- ^ The current output.
  , psNewline     :: !Bool         -- ^ Just outputted a newline?
  , psColumn      :: !Int64        -- ^ Current column.
  , psLine        :: !Int64        -- ^ Current line number.)
  , psUserState   :: !s            -- ^ User state.
  , psExtenders   :: ![Extender s] -- ^ Extenders.
  , psConfig      :: !Config       -- ^ Config which styles may or may
                                   -- not pay attention to.
  }

instance Eq PrintState where
  PrintState ilevel out newline col line _ _ _ == PrintState ilevel' out' newline' col' line' _ _ _ =
    (ilevel,out,newline,col,line) == (ilevel',out',newline',col',line')

-- | A printer extender. Takes as argument the user state that the
-- printer was run with, and the current node to print. Use
-- 'prettyInternal' to fallback to the built-in printer.
data Extender s =
  forall a. (Typeable a) => Extender (s -> a -> Printer ())

-- | A printer style.
data Style =
  forall s. Style {styleName :: !Text
                  ,styleAuthor :: !Text
                  ,styleDescription :: !Text
                  ,styleInitialState :: !s
                  ,styleExtenders :: ![Extender s]
                  ,styleDefConfig :: !Config}

-- | Configurations shared among the different styles. Styles may pay
-- attention to or completely disregard this configuration.
data Config =
  Config {configMaxColumns :: !Int64
         ,configIndentSpaces :: !Int64}

instance Default Config where
  def =
    Config {configMaxColumns = 80
           ,configIndentSpaces = 2}
