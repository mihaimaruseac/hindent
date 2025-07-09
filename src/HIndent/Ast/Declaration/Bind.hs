{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Bind
  ( Bind
  , mkBind
  ) where

import HIndent.Ast.Name.Infix
import HIndent.Ast.Name.Prefix
import HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Types

-- The difference between `Function` and `Pattern` is the same as the difference
-- between `FunBind` and `PatBind` in GHC AST. See
-- https://hackage.haskell.org/package/ghc-lib-parser-9.8.2.20240223/docs/src/Language.Haskell.Syntax.Binds.html.
--
-- TODO: Merge them.
data Bind
  = Function
      { fun_matches :: GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
      }
  | Pattern
      { lhs :: GHC.LPat GHC.GhcPs
      , rhs :: GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
      }
  | PatternSynonym
      { name :: GHC.LIdP GHC.GhcPs
      , parameters :: GHC.HsPatSynDetails GHC.GhcPs
      , isImplicitBidirectional :: Bool
      , explicitMatches :: Maybe
          (GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs))
      , definition :: GHC.LPat GHC.GhcPs
      }

instance CommentExtraction Bind where
  nodeComments Function {} = NodeComments [] [] []
  nodeComments Pattern {} = NodeComments [] [] []
  nodeComments PatternSynonym {} = NodeComments [] [] []

instance Pretty Bind where
  pretty' Function {..} = pretty fun_matches
  pretty' Pattern {..} = pretty lhs >> pretty rhs
  pretty' PatternSynonym {..} = do
    string "pattern "
    case parameters of
      GHC.InfixCon l r ->
        spaced
          [ pretty $ fmap mkPrefixName l
          , pretty $ fmap mkInfixName name
          , pretty $ fmap mkPrefixName r
          ]
      GHC.PrefixCon _ [] -> pretty $ fmap mkPrefixName name
      _ -> spaced [pretty $ fmap mkPrefixName name, pretty parameters]
    let arrow =
          if isImplicitBidirectional
            then "="
            else "<-"
    spacePrefixed [string arrow, pretty $ fmap PatInsidePatDecl definition]
    case explicitMatches of
      Just matches -> do
        newline
        indentedBlock $ string "where " |=> pretty matches
      Nothing -> pure ()

mkBind :: GHC.HsBind GHC.GhcPs -> Bind
mkBind GHC.FunBind {..} = Function {..}
mkBind GHC.PatBind {..} = Pattern {..}
  where
    lhs = pat_lhs
    rhs = pat_rhs
mkBind (GHC.PatSynBind _ GHC.PSB {..}) = PatternSynonym {..}
  where
    name = psb_id
    parameters = psb_args
    (isImplicitBidirectional, explicitMatches) =
      case psb_dir of
        GHC.Unidirectional -> (False, Nothing)
        GHC.ImplicitBidirectional -> (True, Nothing)
        GHC.ExplicitBidirectional matches -> (False, Just matches)
    definition = psb_def
mkBind _ = error "This AST node should not appear."
