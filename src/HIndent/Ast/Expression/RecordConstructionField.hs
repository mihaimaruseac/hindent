{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Expression.RecordConstructionField
  ( RecordConstructionFields
  , mkRecordConstructionFields
  ) where

import Data.Maybe (isJust)
import qualified GHC.Hs as GHC
import HIndent.Ast.NodeComments (NodeComments(..))
import HIndent.Ast.Record.Field (ExprField, mkExprField)
import HIndent.Ast.WithComments (WithComments, fromGenLocated)
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty)
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data RecordConstructionFields = RecordConstructionFields
  { fields :: [WithComments ExprField]
  , dotdot :: Bool
  }

instance CommentExtraction RecordConstructionFields where
  nodeComments _ = NodeComments [] [] []

instance Pretty RecordConstructionFields where
  pretty' RecordConstructionFields {..} =
    hvFields (fmap pretty fields ++ [string ".." | dotdot])

mkRecordConstructionFields ::
     GHC.HsRecordBinds GHC.GhcPs -> RecordConstructionFields
mkRecordConstructionFields GHC.HsRecFields {..} =
  RecordConstructionFields
    { fields = fmap (fmap mkExprField . fromGenLocated) rec_flds
    , dotdot = isJust rec_dotdot
    }
