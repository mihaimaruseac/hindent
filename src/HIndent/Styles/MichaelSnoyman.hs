{-# LANGUAGE OverloadedStrings #-}

-- | Stub module for Michael Snoyman's style.
--
-- Modelled from existing codebases.

module HIndent.Styles.MichaelSnoyman
  (michaelSnoyman)
  where

import HIndent.Instances ()
import HIndent.Types

import Prelude hiding (exp)

-- | Empty state.
data State = State

-- | The printer style.
michaelSnoyman :: Style
michaelSnoyman =
  Style {styleName = "michael-snoyman"
        ,styleAuthor = "Chris Done"
        ,styleDescription = "Style modelled from existing (Yesod, Conduit, etc.) codebases."
        ,styleInitialState = State
        ,styleExtenders = []
        ,styleDefConfig =
           Config {configMaxColumns = 120
                  ,configIndentSpaces = 4}}
