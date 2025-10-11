{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module HIndent.Ast.Match
  ( Match
  , MatchContext
  , LambdaKind
  , FunctionMatch
  , mkExprMatch
  , mkCmdMatch
  ) where

import Control.Monad (unless, when)
import qualified GHC.Hs as GHC
import qualified GHC.Types.Fixity as Fixity
import qualified GHC.Types.SrcLoc as GHC
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
  = Single
  | Case
  | Cases

data FunctionMatch
  = Prefix
      { strictness :: Maybe Strictness
      , name :: WithComments PrefixName
      }
  | Infix
      { strictness :: Maybe Strictness
      , operator :: WithComments InfixName
      }

data MatchContext
  = LambdaContext LambdaKind
  | CaseContext
  | FunctionContext FunctionMatch

data Match = Match
  { context :: MatchContext
  , patterns :: WithComments [WithComments Pattern]
  , rhs :: GuardedRhs
  , needsSpaceAfterLambda :: Bool
  , isCommand :: Bool
  }

instance CommentExtraction Match where
  nodeComments _ = NodeComments [] [] []

instance Pretty Match where
  pretty' Match {context = LambdaContext Single, ..} = do
    string "\\"
    when needsSpaceAfterLambda space
    prettyWith patterns $ spaced . fmap pretty
    when isCommand space
    pretty rhs
  pretty' Match {context = LambdaContext Case, ..} = do
    prettyWith patterns $ spaced . fmap pretty
    when isCommand space
    pretty rhs
  pretty' Match {context = LambdaContext Cases, ..} = do
    prettyWith patterns $ spaced . fmap pretty
    when isCommand space
    pretty rhs
  pretty' Match {context = CaseContext, ..} = do
    prettyWith patterns $ mapM_ pretty
    when isCommand space
    pretty rhs
  pretty' Match {context = FunctionContext Prefix {..}, ..} = do
    maybe (pure ()) pretty strictness
    pretty name
    prettyWith patterns $ \pats ->
      unless (null pats) $ spacePrefixed $ fmap pretty pats
    when isCommand space
    pretty rhs
  pretty' Match {context = FunctionContext Infix {..}, ..} = do
    let render pats =
          case pats of
            (l:r:rest) ->
              spaced $ [pretty l, pretty operator, pretty r] ++ fmap pretty rest
            _ -> error "Not enough parameters are passed."
    prettyWith patterns render
    when isCommand space
    pretty rhs
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
        { context = LambdaContext $ toLambdaKind lamKind
        , patterns = patterns
        , rhs =
            case lamKind of
              GHC.LamSingle -> mkLambdaGuardedRhs grhss
              _ -> mkCaseGuardedRhs grhss
        , needsSpaceAfterLambda =
            case lamKind of
              GHC.LamSingle -> lambdaNeedsSpace rawPatterns
              _ -> False
        , isCommand = False
        }
    mkCase grhss =
      Match
        { context = CaseContext
        , patterns = patterns
        , rhs = mkCaseGuardedRhs grhss
        , needsSpaceAfterLambda = False
        , isCommand = False
        }
    mkFun ctxt grhss =
      Match
        { context = FunctionContext $ mkFunctionMatch ctxt
        , patterns = patterns
        , rhs = mkGuardedRhs grhss
        , needsSpaceAfterLambda = False
        , isCommand = False
        }
    toLambdaKind GHC.LamSingle = Single
    toLambdaKind GHC.LamCase = Case
    toLambdaKind GHC.LamCases = Cases
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkExprMatch :: GHC.Match GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> Match
mkExprMatch match =
  case GHC.m_ctxt match of
    GHC.LamAlt GHC.LamSingle ->
      mkLambda Single (mkLambdaGuardedRhs $ GHC.m_grhss match)
    GHC.LamAlt GHC.LamCase ->
      mkLambda Case (mkCaseGuardedRhs $ GHC.m_grhss match)
    GHC.LamAlt GHC.LamCases ->
      mkLambda Cases (mkCaseGuardedRhs $ GHC.m_grhss match)
    GHC.CaseAlt -> mkCase (mkCaseGuardedRhs $ GHC.m_grhss match)
    ctxt@GHC.FunRhs {} -> mkFun ctxt (mkGuardedRhs $ GHC.m_grhss match)
    _ -> error "`ghc-lib-parser` never generates this AST node."
  where
    rawPatterns = extractPatList (GHC.m_pats match)
    patterns = mkMatchPatterns (GHC.m_pats match)
    mkLambda kind rhs =
      Match
        { context = LambdaContext kind
        , patterns = patterns
        , rhs = rhs
        , needsSpaceAfterLambda =
            case kind of
              Single -> lambdaNeedsSpace rawPatterns
              _ -> False
        , isCommand = False
        }
    mkCase rhs =
      Match
        { context = CaseContext
        , patterns = patterns
        , rhs = rhs
        , needsSpaceAfterLambda = False
        , isCommand = False
        }
    mkFun ctxt rhs =
      Match
        { context = FunctionContext $ mkFunctionMatch ctxt
        , patterns = patterns
        , rhs = rhs
        , needsSpaceAfterLambda = False
        , isCommand = False
        }
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
mkExprMatch :: GHC.Match GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> Match
mkExprMatch match =
  case GHC.m_ctxt match of
    GHC.LambdaExpr -> mkLambda Single (mkLambdaGuardedRhs $ GHC.m_grhss match)
    GHC.LamCaseAlt {} -> mkLambda Case (mkCaseGuardedRhs $ GHC.m_grhss match)
    GHC.CaseAlt -> mkCase (mkCaseGuardedRhs $ GHC.m_grhss match)
    ctxt@GHC.FunRhs {} -> mkFun ctxt (mkGuardedRhs $ GHC.m_grhss match)
    _ -> error "`ghc-lib-parser` never generates this AST node."
  where
    rawPatterns = extractPatList (GHC.m_pats match)
    patterns = mkMatchPatterns (GHC.m_pats match)
    mkLambda kind rhs =
      Match
        { context = LambdaContext kind
        , patterns = patterns
        , rhs = rhs
        , needsSpaceAfterLambda =
            case kind of
              Single -> lambdaNeedsSpace rawPatterns
              _ -> False
        , isCommand = False
        }
    mkCase rhs =
      Match
        { context = CaseContext
        , patterns = patterns
        , rhs = rhs
        , needsSpaceAfterLambda = False
        , isCommand = False
        }
    mkFun ctxt rhs =
      Match
        { context = FunctionContext $ mkFunctionMatch ctxt
        , patterns = patterns
        , rhs = rhs
        , needsSpaceAfterLambda = False
        , isCommand = False
        }
#else
mkExprMatch :: GHC.Match GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> Match
mkExprMatch match =
  case GHC.m_ctxt match of
    GHC.LambdaExpr -> mkLambda Single (mkLambdaGuardedRhs $ GHC.m_grhss match)
    GHC.CaseAlt -> mkCase (mkCaseGuardedRhs $ GHC.m_grhss match)
    ctxt@GHC.FunRhs {} -> mkFun ctxt (mkGuardedRhs $ GHC.m_grhss match)
    _ -> error "`ghc-lib-parser` never generates this AST node."
  where
    rawPatterns = extractPatList (GHC.m_pats match)
    patterns = mkMatchPatterns (GHC.m_pats match)
    mkLambda kind rhs =
      Match
        { context = LambdaContext kind
        , patterns = patterns
        , rhs = rhs
        , needsSpaceAfterLambda =
            case kind of
              Single -> lambdaNeedsSpace rawPatterns
              _ -> False
        , isCommand = False
        }
    mkCase rhs =
      Match
        { context = CaseContext
        , patterns = patterns
        , rhs = rhs
        , needsSpaceAfterLambda = False
        , isCommand = False
        }
    mkFun ctxt rhs =
      Match
        { context = FunctionContext $ mkFunctionMatch ctxt
        , patterns = patterns
        , rhs = rhs
        , needsSpaceAfterLambda = False
        , isCommand = False
        }
#endif

#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkCmdMatch :: GHC.Match GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> Match
mkCmdMatch match =
  case GHC.m_ctxt match of
    GHC.LamAlt lamKind ->
      Match
        { context = LambdaContext $ toLambdaKind lamKind
        , patterns = patterns
        , rhs =
            case lamKind of
              GHC.LamSingle -> mkLambdaCmdGuardedRhs $ GHC.m_grhss match
              _ -> mkCaseCmdGuardedRhs $ GHC.m_grhss match
        , needsSpaceAfterLambda =
            case lamKind of
              GHC.LamSingle -> lambdaNeedsSpace rawPatterns
              _ -> False
        , isCommand = True
        }
    GHC.CaseAlt ->
      Match
        { context = CaseContext
        , patterns = patterns
        , rhs = mkCaseCmdGuardedRhs $ GHC.m_grhss match
        , needsSpaceAfterLambda = False
        , isCommand = True
        }
    _ -> error "`ghc-lib-parser` never generates this AST node."
  where
    rawPatterns = extractPatList (GHC.m_pats match)
    patterns = mkMatchPatterns (GHC.m_pats match)
    toLambdaKind GHC.LamSingle = Single
    toLambdaKind GHC.LamCase = Case
    toLambdaKind GHC.LamCases = Cases
#elif MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkCmdMatch :: GHC.Match GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> Match
mkCmdMatch match =
  case GHC.m_ctxt match of
    GHC.LamAlt GHC.LamSingle ->
      mkLambda Single (mkLambdaCmdGuardedRhs $ GHC.m_grhss match)
    GHC.LamAlt GHC.LamCase ->
      mkLambda Case (mkCaseCmdGuardedRhs $ GHC.m_grhss match)
    GHC.LamAlt GHC.LamCases ->
      mkLambda Cases (mkCaseCmdGuardedRhs $ GHC.m_grhss match)
    GHC.CaseAlt ->
      Match
        { context = CaseContext
        , patterns = patterns
        , rhs = mkCaseCmdGuardedRhs $ GHC.m_grhss match
        , needsSpaceAfterLambda = False
        , isCommand = True
        }
    _ -> error "`ghc-lib-parser` never generates this AST node."
  where
    rawPatterns = extractPatList (GHC.m_pats match)
    patterns = mkMatchPatterns (GHC.m_pats match)
    mkLambda kind rhs =
      Match
        { context = LambdaContext kind
        , patterns = patterns
        , rhs = rhs
        , needsSpaceAfterLambda =
            case kind of
              Single -> lambdaNeedsSpace rawPatterns
              _ -> False
        , isCommand = True
        }
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
mkCmdMatch :: GHC.Match GHC.GhcPs (GHC.LHsCmd GHC.GhcPs) -> Match
mkCmdMatch match =
  case GHC.m_ctxt match of
    GHC.LambdaExpr ->
      Match
        { context = LambdaContext Single
        , patterns = patterns
        , rhs = mkLambdaCmdGuardedRhs $ GHC.m_grhss match
        , needsSpaceAfterLambda = lambdaNeedsSpace rawPatterns
        , isCommand = True
        }
    GHC.LamCaseAlt {} ->
      Match
        { context = LambdaContext Case
        , patterns = patterns
        , rhs = mkCaseCmdGuardedRhs $ GHC.m_grhss match
        , needsSpaceAfterLambda = False
        , isCommand = True
        }
    GHC.CaseAlt ->
      Match
        { context = CaseContext
        , patterns = patterns
        , rhs = mkCaseCmdGuardedRhs $ GHC.m_grhss match
        , needsSpaceAfterLambda = False
        , isCommand = True
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
        { context = LambdaContext Single
        , patterns = patterns
        , rhs = mkLambdaCmdGuardedRhs $ GHC.m_grhss match
        , needsSpaceAfterLambda = lambdaNeedsSpace rawPatterns
        , isCommand = True
        }
    GHC.CaseAlt ->
      Match
        { context = CaseContext
        , patterns = patterns
        , rhs = mkCaseCmdGuardedRhs $ GHC.m_grhss match
        , needsSpaceAfterLambda = False
        , isCommand = True
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
      Prefix
        { strictness = mkStrictness mc_strictness
        , name = mkPrefixName <$> fromGenLocated mc_fun
        }
    Fixity.Infix ->
      Infix
        { strictness = mkStrictness mc_strictness
        , operator = mkInfixName <$> fromGenLocated mc_fun
        }
mkFunctionMatch _ = error "`ghc-lib-parser` never generates this AST node."
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
extractPatList ::
     GHC.XRec GHC.GhcPs [GHC.LPat GHC.GhcPs] -> [GHC.LPat GHC.GhcPs]
extractPatList = GHC.unLoc
#else
extractPatList :: [GHC.LPat GHC.GhcPs] -> [GHC.LPat GHC.GhcPs]
extractPatList = id
#endif
lambdaNeedsSpace :: [GHC.LPat GHC.GhcPs] -> Bool
lambdaNeedsSpace (pat:_) =
  case GHC.unLoc pat of
    GHC.LazyPat {} -> True
    GHC.BangPat {} -> True
    _ -> False
lambdaNeedsSpace _ = False
