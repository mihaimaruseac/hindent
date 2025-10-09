{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Name.RecordField
  ( FieldName
  , mkFieldNameFromFieldOcc
  , mkFieldNameFromAmbiguousFieldOcc
  , mkFieldNameFromFieldLabelStrings
  , mkFieldNameFromLabels
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified GHC.Data.FastString as GHC
import qualified GHC.Hs as GHC
import HIndent.Ast.Name.Prefix (PrefixName, fromString, mkPrefixName)
import HIndent.Ast.NodeComments (NodeComments(..))
import HIndent.Ast.WithComments (WithComments, flattenComments, fromGenLocated)
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty)
import HIndent.Pretty.Combinators (hDotSep)
import HIndent.Pretty.NodeComments
import qualified Language.Haskell.Syntax.Basic as GHC

newtype FieldName =
  FieldName (NonEmpty (WithComments PrefixName))

mkFieldNameFromLabels :: NonEmpty (WithComments PrefixName) -> FieldName
mkFieldNameFromLabels = FieldName

instance CommentExtraction FieldName where
  nodeComments FieldName {} = NodeComments [] [] []

instance Pretty FieldName where
  pretty' (FieldName labels) = hDotSep $ pretty <$> NonEmpty.toList labels

mkFieldNameFromFieldOcc :: GHC.FieldOcc GHC.GhcPs -> FieldName
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
mkFieldNameFromFieldOcc GHC.FieldOcc {..} =
  FieldName $ pure $ mkPrefixName <$> fromGenLocated foLabel
#else
mkFieldNameFromFieldOcc GHC.FieldOcc {..} =
  FieldName $ pure $ mkPrefixName <$> fromGenLocated rdrNameFieldOcc
#endif
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkFieldNameFromAmbiguousFieldOcc :: GHC.FieldOcc GHC.GhcPs -> FieldName
mkFieldNameFromAmbiguousFieldOcc = mkFieldNameFromFieldOcc
#else
mkFieldNameFromAmbiguousFieldOcc :: GHC.AmbiguousFieldOcc GHC.GhcPs -> FieldName
mkFieldNameFromAmbiguousFieldOcc (GHC.Unambiguous GHC.NoExtField name) =
  FieldName $ pure $ mkPrefixName <$> fromGenLocated name
mkFieldNameFromAmbiguousFieldOcc (GHC.Ambiguous GHC.NoExtField name) =
  FieldName $ pure $ mkPrefixName <$> fromGenLocated name
#endif
mkFieldNameFromFieldLabelStrings :: GHC.FieldLabelStrings GHC.GhcPs -> FieldName
mkFieldNameFromFieldLabelStrings (GHC.FieldLabelStrings labels) =
  maybe (error "FieldLabelStrings: expected non-empty label path") FieldName
    $ NonEmpty.nonEmpty
    $ fmap toSegment labels
  where
    toSegment =
      flattenComments
        . fmap
            (fmap (fromString . GHC.unpackFS . GHC.field_label)
               . fromGenLocated
               . GHC.dfoLabel)
        . fromGenLocated
