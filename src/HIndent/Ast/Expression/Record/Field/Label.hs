{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Expression.Record.Field.Label
  ( FieldLabel
  , MkFieldLabel(..)
  ) where

import qualified GHC.Data.FastString as GHC
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Printer
#if MIN_VERSION_ghc_lib_parser(9, 6, 0)
import qualified Language.Haskell.Syntax.Basic as GHC
#elif MIN_VERSION_ghc_lib_parser(9, 4, 0)
import qualified GHC.Types.FieldLabel as GHC
#endif
newtype FieldLabel =
  FieldLabel [Printer ()]

class MkFieldLabel a where
  mkFieldLabel :: a -> FieldLabel

instance CommentExtraction FieldLabel where
  nodeComments FieldLabel {} = NodeComments [] [] []

instance Pretty FieldLabel where
  pretty' (FieldLabel xs) = hDotSep xs
#if MIN_VERSION_ghc_lib_parser(9, 6, 0)
instance MkFieldLabel (GHC.FieldOcc GHC.GhcPs) where
  mkFieldLabel GHC.FieldOcc {..} = FieldLabel [pretty $ fromGenLocated foLabel]

instance MkFieldLabel (GHC.FieldLabelStrings GHC.GhcPs) where
  mkFieldLabel (GHC.FieldLabelStrings xs) =
    FieldLabel
      $ fmap
          (\(GHC.L _ GHC.DotFieldOcc {dfoLabel = GHC.L _ (GHC.FieldLabelString {..})}) ->
             string $ GHC.unpackFS field_label)
          xs
#elif MIN_VERSION_ghc_lib_parser(9, 4, 0)
instance MkFieldLabel (GHC.FieldOcc GHC.GhcPs) where
  mkFieldLabel GHC.FieldOcc {..} = FieldLabel [pretty $ fromGenLocated foLabel]

instance MkFieldLabel (GHC.FieldLabelStrings GHC.GhcPs) where
  mkFieldLabel (GHC.FieldLabelStrings xs) =
    FieldLabel
      $ fmap
          (\(GHC.L _ GHC.DotFieldOcc {dfoLabel = GHC.L _ s}) ->
             string $ GHC.unpackFS s)
          xs
#else
instance MkFieldLabel (GHC.FieldOcc GHC.GhcPs) where
  mkFieldLabel GHC.FieldOcc {..} =
    FieldLabel [pretty $ fromGenLocated rdrNameFieldOcc]

instance MkFieldLabel (GHC.FieldLabelStrings GHC.GhcPs) where
  mkFieldLabel (GHC.FieldLabelStrings xs) =
    FieldLabel
      $ fmap
          (\(GHC.L _ GHC.HsFieldLabel {..}) ->
             prettyWith (fromGenLocated hflLabel $ fmap GHC.unpackFS) string)
          xs

instance MkFieldLabel (GHC.HsFieldLabel GHC.GhcPs) where
  mkFieldLabel GHC.HsFieldLabel {..} =
    FieldLabel [printCommentsAnd hflLabel (string . GHC.unpackFS)]
#endif
instance MkFieldLabel (GHC.AmbiguousFieldOcc GHC.GhcPs) where
  mkFieldLabel (GHC.Unambiguous _ x) = FieldLabel [pretty x]
  mkFieldLabel (GHC.Ambiguous _ x) = FieldLabel [pretty x]

instance (MkFieldLabel a, CommentExtraction l) =>
         MkFieldLabel (GHC.GenLocated l a) where
  mkFieldLabel = FieldLabel . (: []) . pretty . fmap mkFieldLabel
#if MIN_VERSION_ghc_lib_parser(9, 6, 0)
instance MkFieldLabel (GHC.DotFieldOcc GHC.GhcPs) where
  mkFieldLabel GHC.DotFieldOcc {..} =
    let GHC.L _ (GHC.FieldLabelString {..}) = dfoLabel
     in FieldLabel [string $ GHC.unpackFS field_label]
#elif MIN_VERSION_ghc_lib_parser(9, 4, 0)
instance MkFieldLabel (GHC.DotFieldOcc GHC.GhcPs) where
  mkFieldLabel GHC.DotFieldOcc {..} =
    let GHC.L _ s = dfoLabel
     in FieldLabel [string $ GHC.unpackFS s]
#endif
