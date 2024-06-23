{-# LANGUAGE CPP #-}

module HIndent.Ast.Expression.Record.Field
  ( RecordField
  , mkRecordField
  ) where

import                          HIndent.Ast.Expression.Record.Field.Label
import                qualified HIndent.GhcLibParserWrapper.GHC.Hs        as GHC
import {-# SOURCE #-}           HIndent.Pretty
import                          HIndent.Pretty.NodeComments

data RecordField

instance CommentExtraction RecordField

instance Pretty RecordField
#if MIN_VERSION_ghc_lib_parser(9, 4, 0)
mkRecordField ::
     MkFieldLabel a => GHC.HsFieldBind a (GHC.LHsExpr GHC.GhcPs) -> RecordField
#else
mkRecordField ::
     MkFieldLabel a => GHC.HsRecField' a (GHC.LHsExpr GHC.GhcPs) -> RecordField
#endif
