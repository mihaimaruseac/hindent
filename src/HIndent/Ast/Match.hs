{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module HIndent.Ast.Match
  ( Match(..)
  , MatchContext(..)
  , LambdaKind(..)
  , FunctionMatch(..)
  , mkExprMatch
  , mkCmdMatch
  ) where

import Control.Monad (unless, when)
import qualified GHC.Hs as GHC
import qualified GHC.Types.Fixity as Fixity
import qualified GHC.Types.SrcLoc as SrcLoc
import HIndent.Ast.Declaration.Bind.GuardedRhs
  ( GuardedRhs
  , mkCaseCmdGuardedRhs
  , mkCaseGuardedRhs
  , mkGuardedRhs
  , mkLambdaCmdGuardedRhs
  , mkLambdaGuardedRhs
  )
import HIndent.Ast.Name.Infix (InfixName, mkInfixName)
import HIndent.Ast.Name.Prefix (PrefixName, mkPrefixName)
import HIndent.Ast.NodeComments
import HIndent.Ast.Pattern (Pattern, mkPattern)
import HIndent.Ast.Type.Strictness (Strictness, mkStrictness)
import HIndent.Ast.WithComments
  ( WithComments
  , fromGenLocated
  , mkWithComments
  , prettyWith
  )
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
import HIndent.Ast.WithComments (addComments, getComments, getNode)
#endif
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty)
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments (CommentExtraction(..))

data LambdaKind
  = LambdaSingle
  | LambdaCase
  | LambdaCases

data FunctionMatch
  = FunctionPrefix
      { functionStrictness :: Maybe Strictness
      , functionName :: WithComments PrefixName
      }
  | FunctionInfix
      { functionStrictness :: Maybe Strictness
      , functionOperator :: WithComments InfixName
      }

data MatchContext
  = MatchLambda LambdaKind
  | MatchCase
  | MatchFunction FunctionMatch

data Match = Match
  { matchContext :: MatchContext
  , matchPatterns :: WithComments [WithComments Pattern]
  , matchRhs :: GuardedRhs
  , matchLambdaNeedsSpace :: Bool
  , matchIsCommand :: Bool
  }

instance CommentExtraction Match where
  nodeComments _ = NodeComments [] [] []

instance Pretty Match where
  pretty' Match {matchContext = MatchLambda LambdaSingle, ..} = do
    string "\\"
    when matchLambdaNeedsSpace space
    prettyWith matchPatterns $ spaced . fmap pretty
    when matchIsCommand space
    pretty matchRhs
  pretty' Match {matchContext = MatchLambda LambdaCase, ..} = do
    prettyWith matchPatterns $ spaced . fmap pretty
    when matchIsCommand space
    pretty matchRhs
  pretty' Match {matchContext = MatchLambda LambdaCases, ..} = do
    prettyWith matchPatterns $ spaced . fmap pretty
    when matchIsCommand space
    pretty matchRhs
  pretty' Match {matchContext = MatchCase, ..} = do
    prettyWith matchPatterns $ mapM_ pretty
    when matchIsCommand space
    pretty matchRhs
  pretty' Match {matchContext = MatchFunction FunctionPrefix {..}, ..} = do
    maybe (pure ()) pretty functionStrictness
    pretty functionName
    prettyWith matchPatterns $ \pats ->
      unless (null pats) $ spacePrefixed $ fmap pretty pats
    when matchIsCommand space
    pretty matchRhs
  pretty' Match {matchContext = MatchFunction FunctionInfix {..}, ..} = do
    let render pats =
          case pats of
            (l:r:rest) ->
              spaced
                $ [pretty l, pretty functionOperator, pretty r]
                    ++ fmap pretty rest
            _ -> error "Not enough parameters are passed."
    prettyWith matchPatterns render
    when matchIsCommand space
    pretty matchRhs
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkExprMatch :: GHC.Match GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> Match
mkExprMatch match =
  case GHC.m_ctxt match of
    GHC.LamAlt alt -> mkLambda alt (GHC.m_grhss match)
    GHC.CaseAlt -> mkCase (GHC.m_grhss match)
    ctxt@GHC.FunRhs {} -> mkFun ctxt (GHC.m_grhss match)
    _ -> error "`ghc-lib-parser` never generates this AST node."
  where
    rawPatterns = extractPatList (GHC.m_pats match)
    patterns = mkMatchPatterns (GHC.m_pats match)
    mkLambda lamKind grhss =
      Match
        { matchContext = MatchLambda $ toLambdaKind lamKind
        , matchPatterns = patterns
        , matchRhs =
            case lamKind of
              GHC.LamSingle -> mkLambdaGuardedRhs grhss
              _ -> mkCaseGuardedRhs grhss
        , matchLambdaNeedsSpace =
            case lamKind of
              GHC.LamSingle -> lambdaNeedsSpace rawPatterns
              _ -> False
        , matchIsCommand = False
        }
    mkCase grhss =
      Match
        { matchContext = MatchCase
        , matchPatterns = patterns
        , matchRhs = mkCaseGuardedRhs grhss
        , matchLambdaNeedsSpace = False
        , matchIsCommand = False
        }
    mkFun ctxt grhss =
      Match
        { matchContext = MatchFunction $ mkFunctionMatch ctxt
        , matchPatterns = patterns
        , matchRhs = mkGuardedRhs grhss
        , matchLambdaNeedsSpace = False
        , matchIsCommand = False
        }
    toLambdaKind GHC.LamSingle = LambdaSingle
    toLambdaKind GHC.LamCase = LambdaCase
    toLambdaKind GHC.LamCases = LambdaCases
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkExprMatch :: GHC.Match GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> Match
mkExprMatch match =
  case GHC.m_ctxt match of
    GHC.LamAlt GHC.LamSingle ->
      mkLambda LambdaSingle (mkLambdaGuardedRhs $ GHC.m_grhss match)
    GHC.LamAlt GHC.LamCase ->
      mkLambda LambdaCase (mkCaseGuardedRhs $ GHC.m_grhss match)
    GHC.LamAlt GHC.LamCases ->
      mkLambda LambdaCases (mkCaseGuardedRhs $ GHC.m_grhss match)
    GHC.CaseAlt -> mkCase (mkCaseGuardedRhs $ GHC.m_grhss match)
    ctxt@GHC.FunRhs {} -> mkFun ctxt (mkGuardedRhs $ GHC.m_grhss match)
    _ -> error "`ghc-lib-parser` never generates this AST node."
  where
    rawPatterns = extractPatList (GHC.m_pats match)
    patterns = mkMatchPatterns (GHC.m_pats match)
    mkLambda kind rhs =
      Match
        { matchContext = MatchLambda kind
        , matchPatterns = patterns
        , matchRhs = rhs
        , matchLambdaNeedsSpace =
            case kind of
              LambdaSingle -> lambdaNeedsSpace rawPatterns
              _ -> False
        , matchIsCommand = False
        }
    mkCase rhs =
      Match
        { matchContext = MatchCase
        , matchPatterns = patterns
        , matchRhs = rhs
        , matchLambdaNeedsSpace = False
        , matchIsCommand = False
        }
    mkFun ctxt rhs =
      Match
        { matchContext = MatchFunction $ mkFunctionMatch ctxt
        , matchPatterns = patterns
        , matchRhs = rhs
        , matchLambdaNeedsSpace = False
        , matchIsCommand = False
        }
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
mkExprMatch :: GHC.Match GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> Match
mkExprMatch match =
  case GHC.m_ctxt match of
    GHC.LambdaExpr ->
      mkLambda LambdaSingle (mkLambdaGuardedRhs $ GHC.m_grhss match)
    GHC.LamCaseAlt {} ->
      mkLambda LambdaCase (mkCaseGuardedRhs $ GHC.m_grhss match)
    GHC.CaseAlt -> mkCase (mkCaseGuardedRhs $ GHC.m_grhss match)
    ctxt@GHC.FunRhs {} -> mkFun ctxt (mkGuardedRhs $ GHC.m_grhss match)
    _ -> error "`ghc-lib-parser` never generates this AST node."
  where
    rawPatterns = extractPatList (GHC.m_pats match)
    patterns = mkMatchPatterns (GHC.m_pats match)
    mkLambda kind rhs =
      Match
        { matchContext = MatchLambda kind
        , matchPatterns = patterns
        , matchRhs = rhs
        , matchLambdaNeedsSpace =
            case kind of
              LambdaSingle -> lambdaNeedsSpace rawPatterns
              _ -> False
        , matchIsCommand = False
        }
    mkCase rhs =
      Match
        { matchContext = MatchCase
        , matchPatterns = patterns
        , matchRhs = rhs
        , matchLambdaNeedsSpace = False
        , matchIsCommand = False
        }
    mkFun ctxt rhs =
      Match
        { matchContext = MatchFunction $ mkFunctionMatch ctxt
        , matchPatterns = patterns
        , matchRhs = rhs
        , matchLambdaNeedsSpace = False
        , matchIsCommand = False
        }
#else
mkExprMatch :: GHC.Match GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> Match
mkExprMatch match =
  case GHC.m_ctxt match of
    GHC.LambdaExpr ->
      mkLambda LambdaSingle (mkLambdaGuardedRhs $ GHC.m_grhss match)
    GHC.CaseAlt -> mkCase (mkCaseGuardedRhs $ GHC.m_grhss match)
    ctxt@GHC.FunRhs {} -> mkFun ctxt (mkGuardedRhs $ GHC.m_grhss match)
    _ -> error "`ghc-lib-parser` never generates this AST node."
  where
    rawPatterns = extractPatList (GHC.m_pats match)
    patterns = mkMatchPatterns (GHC.m_pats match)
    mkLambda kind rhs =
      Match
        { matchContext = MatchLambda kind
        , matchPatterns = patterns
        , matchRhs = rhs
        , matchLambdaNeedsSpace =
            case kind of
              LambdaSingle -> lambdaNeedsSpace rawPatterns
              _ -> False
        , matchIsCommand = False
        }
    mkCase rhs =
      Match
        { matchContext = MatchCase
        , matchPatterns = patterns
        , matchRhs = rhs
        , matchLambdaNeedsSpace = False
        , matchIsCommand = False
        }
    mkFun ctxt rhs =
      Match
        { matchContext = MatchFunction $ mkFunctionMatch ctxt
        , matchPatterns = patterns
        , matchRhs = rhs
        , matchLambdaNeedsSpace = False
        , matchIsCommand = False
        }
#endif

#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkCmdMatch :: GHC.Match GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> Match
mkCmdMatch match =
  case GHC.m_ctxt match of
    GHC.LamAlt lamKind ->
      Match
        { matchContext = MatchLambda $ toLambdaKind lamKind
        , matchPatterns = patterns
        , matchRhs =
            case lamKind of
              GHC.LamSingle -> mkLambdaCmdGuardedRhs $ GHC.m_grhss match
              _ -> mkCaseCmdGuardedRhs $ GHC.m_grhss match
        , matchLambdaNeedsSpace =
            case lamKind of
              GHC.LamSingle -> lambdaNeedsSpace rawPatterns
              _ -> False
        , matchIsCommand = True
        }
    GHC.CaseAlt ->
      Match
        { matchContext = MatchCase
        , matchPatterns = patterns
        , matchRhs = mkCaseCmdGuardedRhs $ GHC.m_grhss match
        , matchLambdaNeedsSpace = False
        , matchIsCommand = True
        }
    _ -> error "`ghc-lib-parser` never generates this AST node."
  where
    rawPatterns = extractPatList (GHC.m_pats match)
    patterns = mkMatchPatterns (GHC.m_pats match)
    toLambdaKind GHC.LamSingle = LambdaSingle
    toLambdaKind GHC.LamCase = LambdaCase
    toLambdaKind GHC.LamCases = LambdaCases
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkCmdMatch :: GHC.Match GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> Match
mkCmdMatch match =
  case GHC.m_ctxt match of
    GHC.LamAlt GHC.LamSingle ->
      mkLambda LambdaSingle (mkLambdaCmdGuardedRhs $ GHC.m_grhss match)
    GHC.LamAlt GHC.LamCase ->
      mkLambda LambdaCase (mkCaseCmdGuardedRhs $ GHC.m_grhss match)
    GHC.LamAlt GHC.LamCases ->
      mkLambda LambdaCases (mkCaseCmdGuardedRhs $ GHC.m_grhss match)
    GHC.CaseAlt ->
      Match
        { matchContext = MatchCase
        , matchPatterns = patterns
        , matchRhs = mkCaseCmdGuardedRhs $ GHC.m_grhss match
        , matchLambdaNeedsSpace = False
        , matchIsCommand = True
        }
    _ -> error "`ghc-lib-parser` never generates this AST node."
  where
    rawPatterns = extractPatList (GHC.m_pats match)
    patterns = mkMatchPatterns (GHC.m_pats match)
    mkLambda kind rhs =
      Match
        { matchContext = MatchLambda kind
        , matchPatterns = patterns
        , matchRhs = rhs
        , matchLambdaNeedsSpace =
            case kind of
              LambdaSingle -> lambdaNeedsSpace rawPatterns
              _ -> False
        , matchIsCommand = True
        }
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
mkCmdMatch :: GHC.Match GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> Match
mkCmdMatch match =
  case GHC.m_ctxt match of
    GHC.LambdaExpr ->
      Match
        { matchContext = MatchLambda LambdaSingle
        , matchPatterns = patterns
        , matchRhs = mkLambdaCmdGuardedRhs $ GHC.m_grhss match
        , matchLambdaNeedsSpace = lambdaNeedsSpace rawPatterns
        , matchIsCommand = True
        }
    GHC.LamCaseAlt {} ->
      Match
        { matchContext = MatchLambda LambdaCase
        , matchPatterns = patterns
        , matchRhs = mkCaseCmdGuardedRhs $ GHC.m_grhss match
        , matchLambdaNeedsSpace = False
        , matchIsCommand = True
        }
    GHC.CaseAlt ->
      Match
        { matchContext = MatchCase
        , matchPatterns = patterns
        , matchRhs = mkCaseCmdGuardedRhs $ GHC.m_grhss match
        , matchLambdaNeedsSpace = False
        , matchIsCommand = True
        }
    _ -> error "`ghc-lib-parser` never generates this AST node."
  where
    rawPatterns = extractPatList (GHC.m_pats match)
    patterns = mkMatchPatterns (GHC.m_pats match)
#else
mkCmdMatch :: GHC.Match GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> Match
mkCmdMatch match =
  case GHC.m_ctxt match of
    GHC.LambdaExpr ->
      Match
        { matchContext = MatchLambda LambdaSingle
        , matchPatterns = patterns
        , matchRhs = mkLambdaCmdGuardedRhs $ GHC.m_grhss match
        , matchLambdaNeedsSpace = lambdaNeedsSpace rawPatterns
        , matchIsCommand = True
        }
    GHC.CaseAlt ->
      Match
        { matchContext = MatchCase
        , matchPatterns = patterns
        , matchRhs = mkCaseCmdGuardedRhs $ GHC.m_grhss match
        , matchLambdaNeedsSpace = False
        , matchIsCommand = True
        }
    _ -> error "`ghc-lib-parser` never generates this AST node."
  where
    rawPatterns = extractPatList (GHC.m_pats match)
    patterns = mkMatchPatterns (GHC.m_pats match)
#endif

#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkMatchPatterns ::
     GHC.XRec GHC.GhcPs [GHC.LPat GHC.GhcPs]
  -> WithComments [WithComments Pattern]
mkMatchPatterns input =
  let outer = fromGenLocated input
      patterns = fmap (fmap mkPattern . fromGenLocated) (getNode outer)
   in addComments (getComments outer) (mkWithComments patterns)
#else
mkMatchPatterns :: [GHC.LPat GHC.GhcPs] -> WithComments [WithComments Pattern]
mkMatchPatterns = mkWithComments . fmap (fmap mkPattern . fromGenLocated)
#endif
mkFunctionMatch ctx@GHC.FunRhs {..} =
  case GHC.mc_fixity ctx of
    Fixity.Prefix ->
      FunctionPrefix
        { functionStrictness = mkStrictness mc_strictness
        , functionName = mkPrefixName <$> fromGenLocated mc_fun
        }
    Fixity.Infix ->
      FunctionInfix
        { functionStrictness = mkStrictness mc_strictness
        , functionOperator = mkInfixName <$> fromGenLocated mc_fun
        }
mkFunctionMatch _ = error "`ghc-lib-parser` never generates this AST node."
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
extractPatList ::
     GHC.XRec GHC.GhcPs [GHC.LPat GHC.GhcPs] -> [GHC.LPat GHC.GhcPs]
extractPatList = SrcLoc.unLoc
#else
extractPatList :: [GHC.LPat GHC.GhcPs] -> [GHC.LPat GHC.GhcPs]
extractPatList = id
#endif
lambdaNeedsSpace :: [GHC.LPat GHC.GhcPs] -> Bool
lambdaNeedsSpace (pat:_) =
  case SrcLoc.unLoc pat of
    GHC.LazyPat {} -> True
    GHC.BangPat {} -> True
    _ -> False
lambdaNeedsSpace _ = False
