{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Data.Record.Field
  ( RecordField
  , mkRecordField
  ) where

import HIndent.Ast.Name.RecordField (FieldName, mkFieldNameFromFieldOcc)
import {-# SOURCE #-} HIndent.Ast.Type
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators

data RecordField = RecordField
  { names :: [WithComments FieldName]
  , ty :: WithComments Type
  }

instance Pretty RecordField where
  pretty RecordField {..} =
    spaced [hCommaSep $ fmap pretty names, string "::", pretty ty]

#if MIN_VERSION_ghc_lib_parser(9, 14, 0)
mkRecordField :: GHC.HsConDeclRecField GHC.GhcPs -> RecordField
mkRecordField GHC.HsConDeclRecField {..} = RecordField {..}
  where
    names =
      fmap mkFieldNameFromFieldOcc . mkWithCommentsFromGenLocated <$> cdrf_names
    ty = mkTypeFromConDeclField cdrf_spec
#else
mkRecordField :: GHC.ConDeclField GHC.GhcPs -> RecordField
mkRecordField GHC.ConDeclField {..} = RecordField {..}
  where
    names =
      fmap mkFieldNameFromFieldOcc . mkWithCommentsFromGenLocated
        <$> cd_fld_names
    ty = mkType <$> mkWithCommentsFromGenLocated cd_fld_type
#endif
