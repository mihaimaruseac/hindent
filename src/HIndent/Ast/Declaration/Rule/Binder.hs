{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Rule.Binder
  ( RuleBinder
  , mkRuleBinder
  ) where

import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data RuleBinder = RuleBinder
  { name :: GHC.LIdP GHC.GhcPs
  , signature :: Maybe (WithComments (GHC.HsType GHC.GhcPs))
  }

instance CommentExtraction RuleBinder where
  nodeComments RuleBinder {} = NodeComments [] [] []

instance Pretty RuleBinder where
  pretty' RuleBinder {signature = Nothing, ..} = pretty name
  pretty' RuleBinder {signature = Just sig, ..} =
    parens $ spaced [pretty name, string "::", pretty sig]

mkRuleBinder :: GHC.RuleBndr GHC.GhcPs -> RuleBinder
mkRuleBinder (GHC.RuleBndr _ name) = RuleBinder {signature = Nothing, ..}
mkRuleBinder (GHC.RuleBndrSig _ name GHC.HsPS {..}) =
  RuleBinder {signature = Just $ fromGenLocated hsps_body, ..}
