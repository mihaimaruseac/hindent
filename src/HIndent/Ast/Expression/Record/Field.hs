{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Expression.Record.Field
  ( RecordField
  , mkRecordField
  ) where

import HIndent.Ast.Expression
import HIndent.Ast.Expression.Record.Field.Label
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data RecordField = RecordField
  { label :: WithComments FieldLabel
  , expr :: WithComments Expression
  }

instance CommentExtraction RecordField where
  nodeComments RecordField {} = NodeComments [] [] []

instance Pretty RecordField where
  pretty' RecordField {..} = hor <-|> ver
    where
      hor = spaced [pretty label, string "=", pretty expr]
      ver = do
        pretty label
        string " ="
        newline
        indentedBlock $ pretty expr
#if MIN_VERSION_ghc_lib_parser(9, 4, 0)
mkRecordField ::
     MkFieldLabel a => GHC.HsFieldBind a (GHC.LHsExpr GHC.GhcPs) -> RecordField
mkRecordField GHC.HsFieldBind {..} = RecordField {..}
  where
    label = mkWithComments $ mkFieldLabel hfbLHS
    expr = fromGenLocated $ fmap mkExpression hfbRHS
#else
mkRecordField ::
     MkFieldLabel a => GHC.HsRecField' a (GHC.LHsExpr GHC.GhcPs) -> RecordField
mkRecordField GHC.HsRecField {..} = RecordField {..}
  where
    label = fmap mkFieldLabel $ fromGenLocated hsRecFieldLbl
    expr = fromGenLocated $ fmap mkExpression hsRecFieldArg
#endif
