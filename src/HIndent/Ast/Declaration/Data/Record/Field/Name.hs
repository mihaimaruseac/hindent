{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Data.Record.Field.Name
  ( FieldName
  , mkFieldName
  ) where

import qualified GHC.Types.Name.Reader as GHC
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.NodeComments

newtype FieldName =
  FieldName (WithComments GHC.RdrName)

instance CommentExtraction FieldName where
  nodeComments FieldName {} = NodeComments [] [] []

instance Pretty FieldName where
  pretty' (FieldName x) = pretty x

mkFieldName :: GHC.FieldOcc GHC.GhcPs -> FieldName
#if MIN_VERSION_ghc_lib_parser(9, 4, 0)
mkFieldName GHC.FieldOcc {..} = FieldName $ fromGenLocated foLabel
#else
mkFieldName GHC.FieldOcc {..} = FieldName $ fromGenLocated rdrNameFieldOcc
#endif
