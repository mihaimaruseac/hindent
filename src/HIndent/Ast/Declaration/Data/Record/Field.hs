{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Data.Record.Field
  ( RecordField
  , mkRecordField
  ) where

import HIndent.Ast.Name.RecordField (FieldName, mkFieldNameFromFieldOcc)
import HIndent.Ast.NodeComments
import {-# SOURCE #-} HIndent.Ast.Type
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data RecordField = RecordField
  { names :: [WithComments FieldName]
  , ty :: WithComments Type
  }

instance CommentExtraction RecordField where
  nodeComments RecordField {} = NodeComments [] [] []

instance Pretty RecordField where
  pretty' RecordField {..} =
    spaced [hCommaSep $ fmap pretty names, string "::", pretty ty]

mkRecordField :: GHC.ConDeclField GHC.GhcPs -> RecordField
mkRecordField GHC.ConDeclField {..} = RecordField {..}
  where
    names = fmap mkFieldNameFromFieldOcc . fromGenLocated <$> cd_fld_names
    ty = mkType <$> fromGenLocated cd_fld_type
