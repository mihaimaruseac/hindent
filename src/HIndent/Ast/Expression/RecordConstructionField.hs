{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Expression.RecordConstructionField
  ( RecordConstructionFields
  , mkRecordConstructionFields
  ) where

import Data.Maybe (isJust)
import qualified GHC.Hs as GHC
import HIndent.Ast.Record.Field (ExprField, mkExprField)
import HIndent.Ast.WithComments (WithComments, mkWithCommentsFromGenLocated)
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty)
import HIndent.Pretty.Combinators

data RecordConstructionFields = RecordConstructionFields
  { fields :: [WithComments ExprField]
  , dotdot :: Bool
  }

instance Pretty RecordConstructionFields where
  pretty RecordConstructionFields {..} =
    hvFields (fmap pretty fields ++ [string ".." | dotdot])

mkRecordConstructionFields ::
     GHC.HsRecordBinds GHC.GhcPs -> RecordConstructionFields
mkRecordConstructionFields GHC.HsRecFields {..} =
  RecordConstructionFields
    { fields = fmap (fmap mkExprField . mkWithCommentsFromGenLocated) rec_flds
    , dotdot = isJust rec_dotdot
    }
