{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.PatternSynonym
  ( PatternSynonym
  , mkPatternSynonym
  ) where

import HIndent.Applicative
import HIndent.Ast.Declaration.Data.Record.FieldName
import HIndent.Ast.Name.Infix
import HIndent.Ast.Name.Prefix
import HIndent.Ast.Pattern
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data Parameters
  = Prefix
      { args :: [WithComments PrefixName]
      }
  | Infix
      { leftArg :: WithComments PrefixName
      , rightArg :: WithComments PrefixName
      }
  | Record
      { fields :: [WithComments FieldName]
      }

data PatternSynonym = PatternSynonym
  { name :: GHC.LIdP GHC.GhcPs
  , parameters :: WithComments Parameters
  , isImplicitBidirectional :: Bool
  , explicitMatches :: Maybe (GHC.MatchGroup GHC.GhcPs (GHC.LHsExpr GHC.GhcPs))
  , definition :: WithComments PatInsidePatDecl
  }

instance CommentExtraction Parameters where
  nodeComments Prefix {} = emptyNodeComments
  nodeComments Infix {} = emptyNodeComments
  nodeComments Record {} = emptyNodeComments

instance CommentExtraction PatternSynonym where
  nodeComments PatternSynonym {} = emptyNodeComments

instance Pretty Parameters where
  pretty' Prefix {..} = spaced $ fmap pretty args
  pretty' Infix {..} = spaced [pretty leftArg, pretty rightArg]
  pretty' Record {..} = hFields $ fmap pretty fields

instance Pretty PatternSynonym where
  pretty' PatternSynonym {..} = do
    string "pattern "
    case getNode parameters of
      Infix {..} ->
        spaced [pretty leftArg, pretty $ fmap mkInfixName name, pretty rightArg]
      Prefix {args = []} -> pretty $ fmap mkPrefixName name
      _ -> spaced [pretty $ fmap mkPrefixName name, pretty parameters]
    let arrow =
          if isImplicitBidirectional
            then "="
            else "<-"
    spacePrefixed [string arrow, pretty definition]
    whenJust explicitMatches $ \matches -> do
      newline
      indentedBlock $ string "where " |=> pretty matches

mkParameters :: GHC.HsPatSynDetails GHC.GhcPs -> Parameters
mkParameters (GHC.PrefixCon _ args) =
  Prefix {args = map (fromGenLocated . fmap mkPrefixName) args}
mkParameters (GHC.InfixCon l r) =
  Infix
    { leftArg = fromGenLocated $ fmap mkPrefixName l
    , rightArg = fromGenLocated $ fmap mkPrefixName r
    }
mkParameters (GHC.RecCon fields) =
  Record
    {fields = map (mkWithComments . mkFieldName . GHC.recordPatSynField) fields}

mkPatternSynonym :: GHC.PatSynBind GHC.GhcPs GHC.GhcPs -> PatternSynonym
mkPatternSynonym GHC.PSB {..} = PatternSynonym {..}
  where
    name = psb_id
    parameters = mkWithComments $ mkParameters psb_args
    (isImplicitBidirectional, explicitMatches) =
      case psb_dir of
        GHC.Unidirectional -> (False, Nothing)
        GHC.ImplicitBidirectional -> (True, Nothing)
        GHC.ExplicitBidirectional matches -> (False, Just matches)
    definition = mkPatInsidePatDecl <$> fromGenLocated psb_def
