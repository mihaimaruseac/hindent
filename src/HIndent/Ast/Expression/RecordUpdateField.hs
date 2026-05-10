{-# LANGUAGE CPP #-}
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

collectFields :: GHC.LHsRecUpdFields GHC.GhcPs -> [WithComments Field]
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
collectFields GHC.RegularRecUpdFields {..} =
  fmap (fmap mkRegularField . fromGenLocated) recUpdFields
collectFields GHC.OverloadedRecUpdFields {..} =
  fmap (fmap mkOverloadedField . fromGenLocated) olRecUpdFields
#elif MIN_VERSION_ghc_lib_parser(9, 6, 1)
collectFields GHC.RegularRecUpdFields {..} =
  fmap (fmap mkRegularField . fromGenLocated) recUpdFields
collectFields GHC.OverloadedRecUpdFields {..} =
  fmap (fmap mkOverloadedField . fromGenLocated) olRecUpdFields
#else
collectFields = fmap (fmap mkRegularField . fromGenLocated) . either id id
#endif

#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkRegularField ::
     GHC.HsFieldBind (GHC.LFieldOcc GHC.GhcPs) (GHC.LHsExpr GHC.GhcPs) -> Field
mkRegularField GHC.HsFieldBind {..} =
  Field
    { fieldName = mkFieldNameFromFieldOcc <$> fromGenLocated hfbLHS
    , value =
        if hfbPun
          then Nothing
          else Just $ mkExpression <$> fromGenLocated hfbRHS
    }

mkOverloadedField ::
     GHC.HsFieldBind (GHC.LFieldLabelStrings GHC.GhcPs) (GHC.LHsExpr GHC.GhcPs)
  -> Field
mkOverloadedField GHC.HsFieldBind {..} =
  Field
    { fieldName = mkFieldNameFromFieldLabelStrings <$> fromGenLocated hfbLHS
    , value =
        if hfbPun
          then Nothing
          else Just $ mkExpression <$> fromGenLocated hfbRHS
    }
#elif MIN_VERSION_ghc_lib_parser(9, 6, 1)
mkRegularField ::
     GHC.HsFieldBind (GHC.LAmbiguousFieldOcc GHC.GhcPs) (GHC.LHsExpr GHC.GhcPs)
  -> Field
mkRegularField GHC.HsFieldBind {..} =
  Field
    { fieldName = mkFieldNameFromAmbiguousFieldOcc <$> fromGenLocated hfbLHS
    , value =
        if hfbPun
          then Nothing
          else Just $ mkExpression <$> fromGenLocated hfbRHS
    }

mkOverloadedField ::
     GHC.HsFieldBind (GHC.LFieldLabelStrings GHC.GhcPs) (GHC.LHsExpr GHC.GhcPs)
  -> Field
mkOverloadedField GHC.HsFieldBind {..} =
  Field
    { fieldName = mkFieldNameFromFieldLabelStrings <$> fromGenLocated hfbLHS
    , value =
        if hfbPun
          then Nothing
          else Just $ mkExpression <$> fromGenLocated hfbRHS
    }
#else
mkRegularField ::
     GHC.HsFieldBind (GHC.LFieldOcc GHC.GhcPs) (GHC.LHsExpr GHC.GhcPs) -> Field
mkRegularField GHC.HsFieldBind {..} =
  Field
    { fieldName = mkFieldNameFromFieldOcc <$> fromGenLocated hfbLHS
    , value =
        if hfbPun
          then Nothing
          else Just $ mkExpression <$> fromGenLocated hfbRHS
    }
#endif
