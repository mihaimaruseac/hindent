{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module HIndent.Ast.Context
  ( Context
  , mkContext
  ) where

import {-# SOURCE #-} HIndent.Ast.Type
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators

newtype Context =
  Context [WithComments Type]

instance Pretty Context where
  pretty (Context xs) = hor <-|> ver
    where
      hor = parensConditional $ hCommaSep $ fmap pretty xs
        where
          parensConditional =
            case xs of
              [_] -> id
              _ -> parens
      ver =
        case xs of
          [] -> string "()"
          [x] -> pretty x
          _ -> vTuple $ fmap pretty xs

mkContext :: GHC.HsContext GHC.GhcPs -> Context
mkContext = Context . fmap (fmap mkType . mkWithCommentsFromGenLocated)
