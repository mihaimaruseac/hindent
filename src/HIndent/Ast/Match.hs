{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Match
  ( Match
  , mkExprMatch
  , mkCmdMatch
  ) where

import Control.Monad (unless, when)
import qualified GHC.Hs as GHC
import qualified GHC.Types.Fixity as Fixity
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
import qualified GHC.Types.Name.Reader as GHC
#endif
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Applicative (whenJust)
import HIndent.Ast.Declaration.Bind.GuardedRhs
  ( GuardedRhs
  , mkCmdGuardedRhs
  , mkExprGuardedRhs
  )
import HIndent.Ast.Name.Infix (InfixName, mkInfixName)
import HIndent.Ast.Name.Prefix (PrefixName, mkPrefixName)
import HIndent.Ast.Pattern (Pattern, mkPattern)
import HIndent.Ast.Type.Strictness (Strictness, mkStrictness)
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
import HIndent.Ast.WithComments
  ( WithComments
  , mkWithCommentsFromEpaLocated
  , mkWithCommentsFromGenLocated
  , prettyWith
  )
#else
import HIndent.Ast.WithComments
  ( WithComments
  , mkWithComments
  , mkWithCommentsFromGenLocated
  , prettyWith
  )
#endif
import HIndent.Pretty (Pretty(..))
import HIndent.Pretty.Combinators

data InfixOperands = InfixOperands
  { left :: WithComments Pattern
  , operator :: WithComments InfixName
  , right :: WithComments Pattern
  , rest :: [WithComments Pattern]
  }

instance Pretty InfixOperands where
  pretty InfixOperands {..} =
    spaced $ pretty left : pretty operator : pretty right : fmap pretty rest

data Match
  = Lambda
      { needsSpaceAfterLambda :: Bool
      , patterns :: WithComments [WithComments Pattern]
      , rhs :: GuardedRhs
      }
  | Case
      { patterns :: WithComments [WithComments Pattern]
      , rhs :: GuardedRhs
      }
  | FunctionPrefix
      { strictness :: Maybe Strictness
      , name :: WithComments PrefixName
      , patterns :: WithComments [WithComments Pattern]
      , rhs :: GuardedRhs
      }
  | FunctionInfix
      { operands :: WithComments InfixOperands
      , rhs :: GuardedRhs
      }

instance Pretty Match where
  pretty Lambda {..} = do
    string "\\"
    when needsSpaceAfterLambda space
    prettyWith patterns $ spaced . fmap pretty
    pretty rhs
  pretty Case {..} = do
    prettyWith patterns $ spaced . fmap pretty
    pretty rhs
  pretty FunctionPrefix {..} = do
    whenJust strictness pretty
    pretty name
    prettyWith patterns $ \pats ->
      unless (null pats) $ spacePrefixed $ fmap pretty pats
    pretty rhs
  pretty FunctionInfix {..} = do
    pretty operands
    pretty rhs
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkExprMatch :: GHC.Match GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> Match
mkExprMatch match@GHC.Match {GHC.m_ctxt = GHC.LamAlt GHC.LamSingle, ..} =
  Lambda
    { needsSpaceAfterLambda = lambdaNeedsSpace $ GHC.unLoc m_pats
    , rhs = mkExprGuardedRhs match
    , ..
    }
  where
    patterns =
      fmap
        (fmap (mkWithCommentsFromGenLocated . fmap mkPattern))
        (mkWithCommentsFromEpaLocated m_pats)
mkExprMatch match@GHC.Match {GHC.m_ctxt = GHC.LamAlt _, ..} =
  Case
    { rhs = mkExprGuardedRhs match
    , patterns =
        fmap
          (fmap (mkWithCommentsFromGenLocated . fmap mkPattern))
          (mkWithCommentsFromEpaLocated m_pats)
    }
mkExprMatch match@GHC.Match {GHC.m_ctxt = GHC.CaseAlt, ..} =
  Case
    { rhs = mkExprGuardedRhs match
    , patterns =
        fmap
          (fmap (mkWithCommentsFromGenLocated . fmap mkPattern))
          (mkWithCommentsFromEpaLocated m_pats)
    }
mkExprMatch match@GHC.Match {GHC.m_ctxt = ctxt@GHC.FunRhs {}, ..} =
  mkFunctionMatch ctxt patterns (mkExprGuardedRhs match)
  where
    patterns =
      fmap
        (fmap (mkWithCommentsFromGenLocated . fmap mkPattern))
        (mkWithCommentsFromEpaLocated m_pats)
mkExprMatch _ = error "`ghc-lib-parser` never generates this AST node."
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkExprMatch :: GHC.Match GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> Match
mkExprMatch match@GHC.Match {GHC.m_ctxt = GHC.LamAlt GHC.LamSingle, ..} =
  Lambda
    { needsSpaceAfterLambda = lambdaNeedsSpace m_pats
    , patterns =
        mkWithComments
          $ fmap (fmap mkPattern . mkWithCommentsFromGenLocated) m_pats
    , rhs = mkExprGuardedRhs match
    }
mkExprMatch match@GHC.Match {GHC.m_ctxt = GHC.LamAlt GHC.LamCase, ..} =
  Case
    { rhs = mkExprGuardedRhs match
    , patterns =
        mkWithComments
          $ fmap (fmap mkPattern . mkWithCommentsFromGenLocated) m_pats
    }
mkExprMatch match@GHC.Match {GHC.m_ctxt = GHC.LamAlt GHC.LamCases, ..} =
  Case
    { patterns =
        mkWithComments
          $ fmap (fmap mkPattern . mkWithCommentsFromGenLocated) m_pats
    , rhs = mkExprGuardedRhs match
    }
mkExprMatch match@GHC.Match {GHC.m_ctxt = GHC.CaseAlt, ..} =
  Case
    { patterns =
        mkWithComments
          $ fmap (fmap mkPattern . mkWithCommentsFromGenLocated) m_pats
    , rhs = mkExprGuardedRhs match
    }
mkExprMatch match@GHC.Match {GHC.m_ctxt = ctxt@GHC.FunRhs {}, ..} =
  mkFunctionMatch ctxt patterns (mkExprGuardedRhs match)
  where
    patterns =
      mkWithComments
        $ fmap (fmap mkPattern . mkWithCommentsFromGenLocated) m_pats
mkExprMatch _ = error "`ghc-lib-parser` never generates this AST node."
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
mkExprMatch :: GHC.Match GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> Match
mkExprMatch match@GHC.Match {GHC.m_ctxt = GHC.LambdaExpr, ..} =
  Lambda
    { needsSpaceAfterLambda = lambdaNeedsSpace m_pats
    , rhs = mkExprGuardedRhs match
    , ..
    }
  where
    patterns =
      mkWithComments
        $ fmap (fmap mkPattern . mkWithCommentsFromGenLocated) m_pats
mkExprMatch match@GHC.Match {GHC.m_ctxt = GHC.LamCaseAlt {}, ..} =
  Case
    { patterns =
        mkWithComments
          $ fmap (fmap mkPattern . mkWithCommentsFromGenLocated) m_pats
    , rhs = mkExprGuardedRhs match
    }
mkExprMatch match@GHC.Match {GHC.m_ctxt = GHC.CaseAlt, ..} =
  Case
    { patterns =
        mkWithComments
          $ fmap (fmap mkPattern . mkWithCommentsFromGenLocated) m_pats
    , rhs = mkExprGuardedRhs match
    }
mkExprMatch match@GHC.Match {GHC.m_ctxt = ctxt@GHC.FunRhs {}, ..} =
  mkFunctionMatch ctxt patterns (mkExprGuardedRhs match)
  where
    patterns =
      mkWithComments
        $ fmap (fmap mkPattern . mkWithCommentsFromGenLocated) m_pats
mkExprMatch _ = error "`ghc-lib-parser` never generates this AST node."
#else
mkExprMatch :: GHC.Match GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> Match
mkExprMatch match@GHC.Match {GHC.m_ctxt = GHC.LambdaExpr, ..} =
  Lambda
    { needsSpaceAfterLambda = lambdaNeedsSpace m_pats
    , patterns =
        mkWithComments
          $ fmap (fmap mkPattern . mkWithCommentsFromGenLocated) m_pats
    , rhs = mkExprGuardedRhs match
    }
mkExprMatch match@GHC.Match {GHC.m_ctxt = GHC.CaseAlt, ..} =
  Case
    { patterns =
        mkWithComments
          $ fmap (fmap mkPattern . mkWithCommentsFromGenLocated) m_pats
    , rhs = mkExprGuardedRhs match
    }
mkExprMatch match@GHC.Match {GHC.m_ctxt = ctxt@GHC.FunRhs {}, ..} =
  mkFunctionMatch ctxt patterns (mkExprGuardedRhs match)
  where
    patterns =
      mkWithComments
        $ fmap (fmap mkPattern . mkWithCommentsFromGenLocated) m_pats
mkExprMatch _ = error "`ghc-lib-parser` never generates this AST node."
#endif

#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkCmdMatch :: GHC.Match GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> Match
mkCmdMatch match@GHC.Match {GHC.m_ctxt = GHC.LamAlt GHC.LamSingle, ..} =
  Lambda
    { needsSpaceAfterLambda = lambdaNeedsSpace $ GHC.unLoc m_pats
    , rhs = mkCmdGuardedRhs match
    , ..
    }
  where
    patterns =
      fmap
        (fmap (mkWithCommentsFromGenLocated . fmap mkPattern))
        (mkWithCommentsFromEpaLocated m_pats)
mkCmdMatch match@GHC.Match {GHC.m_ctxt = GHC.LamAlt _, ..} =
  Case
    { patterns =
        fmap
          (fmap (mkWithCommentsFromGenLocated . fmap mkPattern))
          (mkWithCommentsFromEpaLocated m_pats)
    , rhs = mkCmdGuardedRhs match
    }
mkCmdMatch match@GHC.Match {GHC.m_ctxt = GHC.CaseAlt, ..} =
  Case
    { patterns =
        fmap
          (fmap (mkWithCommentsFromGenLocated . fmap mkPattern))
          (mkWithCommentsFromEpaLocated m_pats)
    , rhs = mkCmdGuardedRhs match
    }
mkCmdMatch _ = error "`ghc-lib-parser` never generates this AST node."
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkCmdMatch :: GHC.Match GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> Match
mkCmdMatch match@GHC.Match {GHC.m_ctxt = GHC.LamAlt GHC.LamSingle, ..} =
  Lambda
    { needsSpaceAfterLambda = lambdaNeedsSpace m_pats
    , patterns =
        mkWithComments
          $ fmap mkPattern . mkWithCommentsFromGenLocated <$> m_pats
    , rhs = mkCmdGuardedRhs match
    }
mkCmdMatch match@GHC.Match {GHC.m_ctxt = GHC.LamAlt GHC.LamCase, ..} =
  Case
    { patterns =
        mkWithComments
          $ fmap mkPattern . mkWithCommentsFromGenLocated <$> m_pats
    , rhs = mkCmdGuardedRhs match
    }
mkCmdMatch match@GHC.Match {GHC.m_ctxt = GHC.LamAlt GHC.LamCases, ..} =
  Case
    { patterns =
        mkWithComments
          $ fmap mkPattern . mkWithCommentsFromGenLocated <$> m_pats
    , rhs = mkCmdGuardedRhs match
    }
mkCmdMatch match@GHC.Match {GHC.m_ctxt = GHC.CaseAlt, ..} =
  Case
    { patterns =
        mkWithComments
          $ fmap mkPattern . mkWithCommentsFromGenLocated <$> m_pats
    , rhs = mkCmdGuardedRhs match
    }
mkCmdMatch _ = error "`ghc-lib-parser` never generates this AST node."
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
mkCmdMatch :: GHC.Match GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> Match
mkCmdMatch match@GHC.Match {GHC.m_ctxt = GHC.LambdaExpr, ..} =
  Lambda
    { needsSpaceAfterLambda = lambdaNeedsSpace m_pats
    , patterns =
        mkWithComments
          $ fmap mkPattern . mkWithCommentsFromGenLocated <$> m_pats
    , rhs = mkCmdGuardedRhs match
    }
mkCmdMatch match@GHC.Match {GHC.m_ctxt = GHC.LamCaseAlt {}, ..} =
  Case
    { patterns =
        mkWithComments
          $ fmap mkPattern . mkWithCommentsFromGenLocated <$> m_pats
    , rhs = mkCmdGuardedRhs match
    }
mkCmdMatch match@GHC.Match {GHC.m_ctxt = GHC.CaseAlt, ..} =
  Case
    { patterns =
        mkWithComments
          $ fmap mkPattern . mkWithCommentsFromGenLocated <$> m_pats
    , rhs = mkCmdGuardedRhs match
    }
mkCmdMatch _ = error "`ghc-lib-parser` never generates this AST node."
#else
mkCmdMatch :: GHC.Match GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> Match
mkCmdMatch match@GHC.Match {GHC.m_ctxt = GHC.LambdaExpr, ..} =
  Lambda
    { needsSpaceAfterLambda = lambdaNeedsSpace m_pats
    , patterns =
        mkWithComments
          $ fmap mkPattern . mkWithCommentsFromGenLocated <$> m_pats
    , rhs = mkCmdGuardedRhs match
    }
mkCmdMatch match@GHC.Match {GHC.m_ctxt = GHC.CaseAlt, ..} =
  Case
    { rhs = mkCmdGuardedRhs match
    , patterns =
        mkWithComments
          $ fmap mkPattern . mkWithCommentsFromGenLocated <$> m_pats
    }
mkCmdMatch _ = error "`ghc-lib-parser` never generates this AST node."
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkFunctionMatch ::
     GHC.HsMatchContext (GHC.GenLocated GHC.SrcSpanAnnN GHC.RdrName)
  -> WithComments [WithComments Pattern]
  -> GuardedRhs
  -> Match
#else
mkFunctionMatch ::
     GHC.HsMatchContext GHC.GhcPs
  -> WithComments [WithComments Pattern]
  -> GuardedRhs
  -> Match
#endif
mkFunctionMatch GHC.FunRhs {mc_fixity = Fixity.Prefix, ..} patterns rhs =
  FunctionPrefix
    { strictness = mkStrictness mc_strictness
    , name = mkPrefixName <$> mkWithCommentsFromGenLocated mc_fun
    , ..
    }
mkFunctionMatch GHC.FunRhs {mc_fixity = Fixity.Infix, ..} pats rhs =
  FunctionInfix {..}
  where
    operands =
      flip fmap pats $ \case
        left:right:rest ->
          InfixOperands
            {operator = mkInfixName <$> mkWithCommentsFromGenLocated mc_fun, ..}
        _ -> error "FunctionInfix match must have at least two patterns."
mkFunctionMatch _ _ _ = error "`ghc-lib-parser` never generates this AST node."

lambdaNeedsSpace :: [GHC.LPat GHC.GhcPs] -> Bool
lambdaNeedsSpace (pat:_) =
  case GHC.unLoc pat of
    GHC.LazyPat {} -> True
    GHC.BangPat {} -> True
    _ -> False
lambdaNeedsSpace _ = False
