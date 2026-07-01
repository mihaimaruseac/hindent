{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.PatternSynonym
  ( PatternSynonym
  , mkPatternSynonym
  ) where

import HIndent.Applicative
import HIndent.Ast.MatchGroup (MatchGroup, mkExprMatchGroup)
import HIndent.Ast.Name.Infix
import HIndent.Ast.Name.Prefix
import HIndent.Ast.Name.RecordField (FieldName, mkFieldNameFromFieldOcc)
import HIndent.Ast.Pattern
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Printer

data PatternSynonym
  = Prefix
      { name :: WithComments PrefixName
      , args :: [WithComments PrefixName]
      , isImplicitBidirectional :: Bool
      , explicitMatches :: Maybe MatchGroup
      , definition :: WithComments PatInsidePatDecl
      }
  | Infix
      { leftArg :: WithComments PrefixName
      , operator :: WithComments InfixName
      , rightArg :: WithComments PrefixName
      , isImplicitBidirectional :: Bool
      , explicitMatches :: Maybe MatchGroup
      , definition :: WithComments PatInsidePatDecl
      }
  | Record
      { name :: WithComments PrefixName
      , fields :: [WithComments FieldName]
      , isImplicitBidirectional :: Bool
      , explicitMatches :: Maybe MatchGroup
      , definition :: WithComments PatInsidePatDecl
      }

instance Pretty PatternSynonym where
  pretty Prefix {..} = do
    string "pattern "
    spaced $ pretty name : fmap pretty args
    prettySuffix isImplicitBidirectional definition explicitMatches
  pretty Infix {..} = do
    string "pattern "
    spaced [pretty leftArg, pretty operator, pretty rightArg]
    prettySuffix isImplicitBidirectional definition explicitMatches
  pretty Record {..} = do
    string "pattern "
    spaced [pretty name, hFields $ fmap pretty fields]
    prettySuffix isImplicitBidirectional definition explicitMatches

mkPatternSynonym :: GHC.PatSynBind GHC.GhcPs GHC.GhcPs -> PatternSynonym
#if MIN_VERSION_ghc_lib_parser(9, 14, 0)
mkPatternSynonym GHC.PSB {GHC.psb_args = GHC.PrefixCon prefixArgs, ..} =
  Prefix
    { name = mkWithCommentsFromGenLocated $ mkPrefixName <$> psb_id
    , args = map (mkWithCommentsFromGenLocated . fmap mkPrefixName) prefixArgs
    , definition = mkPatInsidePatDecl <$> mkWithCommentsFromGenLocated psb_def
    , ..
    }
  where
    (isImplicitBidirectional, explicitMatches) =
      case psb_dir of
        GHC.Unidirectional -> (False, Nothing)
        GHC.ImplicitBidirectional -> (True, Nothing)
        GHC.ExplicitBidirectional matches ->
          (False, Just $ mkExprMatchGroup matches)
mkPatternSynonym GHC.PSB {GHC.psb_args = GHC.InfixCon leftArg rightArg, ..} =
  Infix
    { leftArg = mkWithCommentsFromGenLocated $ mkPrefixName <$> leftArg
    , operator = mkWithCommentsFromGenLocated $ mkInfixName <$> psb_id
    , rightArg = mkWithCommentsFromGenLocated $ mkPrefixName <$> rightArg
    , definition = mkPatInsidePatDecl <$> mkWithCommentsFromGenLocated psb_def
    , ..
    }
  where
    (isImplicitBidirectional, explicitMatches) =
      case psb_dir of
        GHC.Unidirectional -> (False, Nothing)
        GHC.ImplicitBidirectional -> (True, Nothing)
        GHC.ExplicitBidirectional matches ->
          (False, Just $ mkExprMatchGroup matches)
mkPatternSynonym GHC.PSB {GHC.psb_args = GHC.RecCon recordFields, ..} =
  Record
    { name = mkWithCommentsFromGenLocated $ mkPrefixName <$> psb_id
    , fields =
        map
          (mkWithComments . mkFieldNameFromFieldOcc . GHC.recordPatSynField)
          recordFields
    , definition = mkPatInsidePatDecl <$> mkWithCommentsFromGenLocated psb_def
    , ..
    }
  where
    (isImplicitBidirectional, explicitMatches) =
      case psb_dir of
        GHC.Unidirectional -> (False, Nothing)
        GHC.ImplicitBidirectional -> (True, Nothing)
        GHC.ExplicitBidirectional matches ->
          (False, Just $ mkExprMatchGroup matches)
#else
mkPatternSynonym GHC.PSB {GHC.psb_args = GHC.PrefixCon _ prefixArgs, ..} =
  Prefix
    { name = mkWithCommentsFromGenLocated $ mkPrefixName <$> psb_id
    , args = map (mkWithCommentsFromGenLocated . fmap mkPrefixName) prefixArgs
    , definition = mkPatInsidePatDecl <$> mkWithCommentsFromGenLocated psb_def
    , ..
    }
  where
    (isImplicitBidirectional, explicitMatches) =
      case psb_dir of
        GHC.Unidirectional -> (False, Nothing)
        GHC.ImplicitBidirectional -> (True, Nothing)
        GHC.ExplicitBidirectional matches ->
          (False, Just $ mkExprMatchGroup matches)
mkPatternSynonym GHC.PSB {GHC.psb_args = GHC.InfixCon leftArg rightArg, ..} =
  Infix
    { leftArg = mkWithCommentsFromGenLocated $ mkPrefixName <$> leftArg
    , operator = mkWithCommentsFromGenLocated $ mkInfixName <$> psb_id
    , rightArg = mkWithCommentsFromGenLocated $ mkPrefixName <$> rightArg
    , definition = mkPatInsidePatDecl <$> mkWithCommentsFromGenLocated psb_def
    , ..
    }
  where
    (isImplicitBidirectional, explicitMatches) =
      case psb_dir of
        GHC.Unidirectional -> (False, Nothing)
        GHC.ImplicitBidirectional -> (True, Nothing)
        GHC.ExplicitBidirectional matches ->
          (False, Just $ mkExprMatchGroup matches)
mkPatternSynonym GHC.PSB {GHC.psb_args = GHC.RecCon recordFields, ..} =
  Record
    { name = mkWithCommentsFromGenLocated $ mkPrefixName <$> psb_id
    , fields =
        map
          (mkWithComments . mkFieldNameFromFieldOcc . GHC.recordPatSynField)
          recordFields
    , definition = mkPatInsidePatDecl <$> mkWithCommentsFromGenLocated psb_def
    , ..
    }
  where
    (isImplicitBidirectional, explicitMatches) =
      case psb_dir of
        GHC.Unidirectional -> (False, Nothing)
        GHC.ImplicitBidirectional -> (True, Nothing)
        GHC.ExplicitBidirectional matches ->
          (False, Just $ mkExprMatchGroup matches)
#endif
prettySuffix ::
     Bool -> WithComments PatInsidePatDecl -> Maybe MatchGroup -> Printer ()
prettySuffix isImplicitBidirectional definition explicitMatches = do
  let arrow =
        if isImplicitBidirectional
          then "="
          else "<-"
  spacePrefixed [string arrow, pretty definition]
  whenJust explicitMatches $ \matches -> do
    newline
    indentedBlock $ string "where " |=> pretty matches
