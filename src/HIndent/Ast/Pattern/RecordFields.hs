{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Pattern.RecordFields
  ( RecordFieldsPat
  , mkRecordFieldsPat
  ) where

import Control.Monad (unless)
import Data.Maybe (maybeToList)
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.Name.RecordField (FieldName, mkFieldNameFromFieldOcc)
import HIndent.Ast.NodeComments
import {-# SOURCE #-} HIndent.Ast.Pattern (mkPattern)
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data RecordField = RecordField
  { fieldName :: WithComments FieldName
  , fieldValue :: GHC.GenLocated GHC.SrcSpanAnnA (GHC.Pat GHC.GhcPs)
  , isPun :: Bool
  }

data RecordFieldsPat = RecordFieldsPat
  { fields :: [WithComments RecordField]
  , dotdot :: Maybe (WithComments GHC.RecFieldsDotDot)
  }

instance CommentExtraction RecordField where
  nodeComments RecordField {} = NodeComments [] [] []

instance CommentExtraction RecordFieldsPat where
  nodeComments RecordFieldsPat {} = NodeComments [] [] []

instance Pretty RecordFieldsPat where
  pretty' (RecordFieldsPat fs dd) =
    case fieldPrinters of
      [] -> string "{}"
      [x] -> braces x
      xs -> hvFields xs
    where
      fieldPrinters =
        fmap pretty fs ++ maybeToList (fmap (const (string "..")) dd)

instance Pretty RecordField where
  pretty' RecordField {..} = do
    pretty fieldName
    unless isPun $ do
      string " = "
      pretty $ mkPattern <$> fromGenLocated fieldValue
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
mkRecordField ::
     GHC.HsFieldBind (GHC.LFieldOcc GHC.GhcPs) (GHC.LPat GHC.GhcPs)
  -> RecordField
mkRecordField GHC.HsFieldBind {..} =
  RecordField
    { fieldName = mkFieldNameFromFieldOcc <$> fromGenLocated hfbLHS
    , fieldValue = hfbRHS
    , isPun = hfbPun
    }
#else
mkRecordField ::
     GHC.HsRecField' (GHC.FieldOcc GHC.GhcPs) (GHC.LPat GHC.GhcPs)
  -> RecordField
mkRecordField GHC.HsRecField {..} =
  RecordField
    { fieldName = mkFieldNameFromFieldOcc <$> fromGenLocated hsRecFieldLbl
    , fieldValue = hsRecFieldArg
    , isPun = hsRecPun
    }
#endif
mkRecordFieldsPat ::
     GHC.HsRecFields GHC.GhcPs (GHC.LPat GHC.GhcPs) -> RecordFieldsPat
mkRecordFieldsPat GHC.HsRecFields {..} =
  RecordFieldsPat
    { fields = map (fmap mkRecordField . fromGenLocated) rec_flds
    , dotdot = fmap fromGenLocated rec_dotdot
    }
