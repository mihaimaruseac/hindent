{-# LANGUAGE OverloadedStrings #-}

module HIndent.Ast.Expression.DoOrMdo
  ( DoOrMdo
  , mkDoOrMdo
  ) where

import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty (Pretty(..))
import HIndent.Pretty.Combinators

data DoOrMdo
  = Do
  | Mdo

instance Pretty DoOrMdo where
  pretty Do = string "do"
  pretty Mdo = string "mdo"

mkDoOrMdo :: GHC.HsDoFlavour -> DoOrMdo
mkDoOrMdo (GHC.DoExpr _) = Do
mkDoOrMdo (GHC.MDoExpr _) = Mdo
mkDoOrMdo _ = error "`mkDoOrMdo` only supports `DoExpr` and `MDoExpr`."
