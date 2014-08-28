{-# LANGUAGE OverloadedStrings #-}

-- | Chris Done's style.
--
-- Documented here: <https://github.com/chrisdone/haskell-style-guide>

module HIndent.Styles.ChrisDone
  (chrisdone)
  where

import HIndent.Types

-- | The printer style.
chrisdone :: Style
chrisdone =
  Style {styleAuthor = "Chris Done"
        ,styleInitialState = ()
        ,styleExtenders = []}
