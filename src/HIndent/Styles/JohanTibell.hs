{-# LANGUAGE OverloadedStrings #-}

-- | Stub module for Johan Tibell's style.
--
-- Documented here: <https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md>

module HIndent.Styles.JohanTibell
  (johanTibell)
  where

import HIndent.Instances ()
import HIndent.Types

import Prelude hiding (exp)

-- | Empty state.
data State = State

-- | The printer style.
johanTibell :: Style
johanTibell =
  Style {styleName = "johan-tibell"
        ,styleAuthor = "Chris Done"
        ,styleDescription = "Style modeled from Johan's style guide here: <https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md>"
        ,styleInitialState = State
        ,styleExtenders = []
        ,styleDefConfig =
           Config {configMaxColumns = 80
                  ,configIndentSpaces = 4}}
