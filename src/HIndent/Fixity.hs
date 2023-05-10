-- | Operator fixities.
--
-- It is very difficult to take operators' fixities into account as fixity
-- information is not stored in an AST. While `ghc-lib-parser-ex` provides
-- `fixitiesFromModule`, it is almost useless as operators are usually imported
-- from other modules.
module HIndent.Fixity
  ( fixities
  ) where

import GHC.Types.Fixity
import Language.Haskell.GhclibParserEx.Fixity

fixities :: [(String, Fixity)]
fixities = baseFixities <> lensFixities

lensFixities :: [(String, Fixity)]
lensFixities =
  concat
    [ infixr_
        4
        [ ".~"
        , "%~"
        , "+~"
        , "-~"
        , "*~"
        , "//~"
        , "^~"
        , "^^~"
        , "**~"
        , "||~"
        , "<>~"
        , "&&~"
        , "<.~"
        , "?~"
        , "<?~"
        , "%@~"
        , ".@~"
        ]
    , infix_
        4
        [ ".="
        , "%="
        , "+="
        , "-="
        , "*="
        , "//="
        , "^="
        , "^^="
        , "**="
        , "||="
        , "<>="
        , "&&="
        , "<.="
        , "?="
        , "<?="
        , "%@="
        , ".@="
        ]
    , infixr_ 2 ["<^"]
    , infixl_ 1 ["&"]
    ]
