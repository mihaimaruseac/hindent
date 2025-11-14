{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Pattern.RecordFields
  ( RecordFieldsPat
  , mkRecordFieldsPat
  ) where

import Data.Maybe (isJust)
import HIndent.Ast.NodeComments
import HIndent.Ast.Record.Field (PatField, mkPatField)
import HIndent.Ast.WithComments (WithComments, fromGenLocated)
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty)
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data RecordFieldsPat = RecordFieldsPat
  { fields :: [WithComments PatField]
  , dotdot :: Bool
  }

instance CommentExtraction RecordFieldsPat where
  nodeComments RecordFieldsPat {} = NodeComments [] [] []

instance Pretty RecordFieldsPat where
  pretty' (RecordFieldsPat fs dd) =
    case fieldPrinters of
      [] -> string "{}"
      [x] -> braces x
      xs -> hvFields xs
    where
      fieldPrinters = fmap pretty fs ++ [string ".." | dd]

mkRecordFieldsPat ::
     GHC.HsRecFields GHC.GhcPs (GHC.LPat GHC.GhcPs) -> RecordFieldsPat
mkRecordFieldsPat GHC.HsRecFields {..} =
  RecordFieldsPat
    { fields = fmap (fmap mkPatField . fromGenLocated) rec_flds
    , dotdot = isJust rec_dotdot
    }
