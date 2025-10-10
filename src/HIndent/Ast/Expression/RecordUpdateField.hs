{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Expression.RecordUpdateField
  ( RecordUpdateFields
  , mkRecordUpdateFields
  ) where

import qualified GHC.Hs as GHC
import HIndent.Applicative (whenJust)
import {-# SOURCE #-} HIndent.Ast.Expression (Expression, mkExpression)
import HIndent.Ast.NodeComments (NodeComments(..))
import HIndent.Ast.WithComments
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty)
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Printer (Printer)
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
import HIndent.Ast.Name.RecordField
  ( FieldName
  , mkFieldNameFromFieldLabelStrings
  , mkFieldNameFromFieldOcc
  )
#elif MIN_VERSION_ghc_lib_parser(9, 6, 1)
import HIndent.Ast.Name.RecordField
  ( FieldName
  , mkFieldNameFromAmbiguousFieldOcc
  , mkFieldNameFromFieldLabelStrings
  )
#else
import HIndent.Ast.Name.RecordField
  ( FieldName
  , mkFieldNameFromFieldLabelStrings
  , mkFieldNameFromFieldOcc
  )
#endif
data RecordUpdateFields = RecordUpdateFields
  { expression :: WithComments Expression
  , fields :: [WithComments Field]
  }

instance CommentExtraction RecordUpdateFields where
  nodeComments _ = NodeComments [] [] []

instance Pretty RecordUpdateFields where
  pretty' RecordUpdateFields {..} = horizontal <-|> vertical
    where
      horizontal =
        spaced
          [ pretty expression
          , hFields $ fmap (`prettyWith` prettyFieldHorizontal) fields
          ]
      vertical = do
        pretty expression
        newline
        indentedBlock
          $ hFields (fmap (`prettyWith` prettyFieldHorizontal) fields)
              <-|> vFields (fmap (`prettyWith` prettyFieldVertical) fields)

mkRecordUpdateFields ::
     WithComments Expression
  -> GHC.LHsRecUpdFields GHC.GhcPs
  -> RecordUpdateFields
mkRecordUpdateFields expression updates =
  RecordUpdateFields {fields = collectFields updates, ..}

data Field = Field
  { fieldName :: WithComments FieldName
  , value :: Maybe (WithComments Expression)
  }

instance CommentExtraction Field where
  nodeComments _ = NodeComments [] [] []

prettyFieldHorizontal :: Field -> Printer ()
prettyFieldHorizontal Field {..} = do
  pretty fieldName
  whenJust value $ \val -> do
    string " = "
    pretty val

prettyFieldVertical :: Field -> Printer ()
prettyFieldVertical Field {..} = do
  pretty fieldName
  whenJust value $ \val -> do
    string " ="
    (space >> pretty val) <-|> (newline >> indentedBlock (pretty val))

collectFieldsWith ::
     CommentExtraction (GHC.Anno label)
  => (label -> FieldName)
  -> [GHC.LocatedA
        (GHC.HsFieldBind (GHC.XRec GHC.GhcPs label) (GHC.LHsExpr GHC.GhcPs))]
  -> [WithComments Field]
collectFieldsWith mkLabel =
  fmap (fmap (mkField (fmap mkLabel . fromGenLocated)) . fromGenLocated)

collectFields :: GHC.LHsRecUpdFields GHC.GhcPs -> [WithComments Field]
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
collectFields GHC.RegularRecUpdFields {..} =
  collectFieldsWith mkFieldNameFromFieldOcc recUpdFields
collectFields GHC.OverloadedRecUpdFields {..} =
  collectFieldsWith mkFieldNameFromFieldLabelStrings olRecUpdFields
#elif MIN_VERSION_ghc_lib_parser(9, 6, 1)
collectFields GHC.RegularRecUpdFields {..} =
  collectFieldsWith mkFieldNameFromAmbiguousFieldOcc recUpdFields
collectFields GHC.OverloadedRecUpdFields {..} =
  collectFieldsWith mkFieldNameFromFieldLabelStrings olRecUpdFields
#else
collectFields = collectFieldsWith mkFieldNameFromFieldOcc . either id id
#endif
mkField ::
     (GHC.XRec GHC.GhcPs label -> WithComments FieldName)
  -> GHC.HsFieldBind (GHC.XRec GHC.GhcPs label) (GHC.LHsExpr GHC.GhcPs)
  -> Field
mkField wrapLabel GHC.HsFieldBind {..} =
  Field
    { fieldName = wrapLabel hfbLHS
    , value =
        if hfbPun
          then Nothing
          else Just $ mkExpression <$> fromGenLocated hfbRHS
    }
