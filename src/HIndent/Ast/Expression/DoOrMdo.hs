{-# LANGUAGE OverloadedStrings #-}

module HIndent.Ast.Expression.DoOrMdo
  ( DoOrMdo
  , mkDoOrMdo
  ) where

import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty (Pretty(..))
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data DoOrMdo
  = Do
  | Mdo

instance CommentExtraction DoOrMdo where
  nodeComments = const emptyNodeComments

instance Pretty DoOrMdo where
  pretty' Do = string "do"
  pretty' Mdo = string "mdo"

mkDoOrMdo :: GHC.HsDoFlavour -> DoOrMdo
mkDoOrMdo (GHC.DoExpr _) = Do
mkDoOrMdo (GHC.MDoExpr _) = Mdo
mkDoOrMdo _ = error "`mkDoOrMdo` only supports `DoExpr` and `MDoExpr`."
