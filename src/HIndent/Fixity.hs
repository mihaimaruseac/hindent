-- | Operator fixities.
--
-- It is very difficult to take operators' fixities into account as fixity
-- information is not stored in an AST. While `ghc-lib-parser-ex` provides
-- `fixitiesFromModule`, it is almost useless as operators are usually imported
-- from other modules.
--
-- Ormolu is trying to resolve this issue by examing Hackage, but doing the same
-- way in HIndent is not so easy.
module HIndent.Fixity
  ( fixities
  ) where

import GHC.Types.Fixity
import Language.Haskell.GhclibParserEx.Fixity

-- | Operator fixities that HIndent supports.
fixities :: [(String, Fixity)]
fixities = baseFixities <> lensFixities

-- | Fixities of operators defined in lens package.
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
