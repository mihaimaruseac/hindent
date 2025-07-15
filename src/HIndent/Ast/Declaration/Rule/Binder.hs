{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Rule.Binder
  ( RuleBinder
  , mkRuleBinder
  ) where

import HIndent.Ast.Name.Prefix
import HIndent.Ast.NodeComments
import HIndent.Ast.Type
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data RuleBinder = RuleBinder
  { name :: WithComments PrefixName
  , signature :: Maybe (WithComments Type)
  }

instance CommentExtraction RuleBinder where
  nodeComments RuleBinder {} = NodeComments [] [] []

instance Pretty RuleBinder where
  pretty' RuleBinder {signature = Nothing, ..} = pretty name
  pretty' RuleBinder {signature = Just sig, ..} =
    parens $ spaced [pretty name, string "::", pretty sig]

mkRuleBinder :: GHC.RuleBndr GHC.GhcPs -> RuleBinder
mkRuleBinder (GHC.RuleBndr _ n) = RuleBinder {..}
  where
    signature = Nothing
    name = fromGenLocated $ fmap mkPrefixName n
mkRuleBinder (GHC.RuleBndrSig _ n GHC.HsPS {..}) = RuleBinder {..}
  where
    signature = Just $ fromGenLocated $ fmap mkType hsps_body
    name = fromGenLocated $ fmap mkPrefixName n
