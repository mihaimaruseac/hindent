{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Data.Record.FieldName
  ( FieldName
  , mkFieldName
  ) where

import HIndent.Ast.Name.Prefix
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.NodeComments

newtype FieldName =
  FieldName (WithComments PrefixName)

instance CommentExtraction FieldName where
  nodeComments (FieldName _) = NodeComments [] [] []

instance Pretty FieldName where
  pretty' (FieldName name) = pretty name

mkFieldName :: GHC.FieldOcc GHC.GhcPs -> FieldName
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
mkFieldName GHC.FieldOcc {..} =
  FieldName $ mkPrefixName <$> fromGenLocated foLabel
#else
mkFieldName GHC.FieldOcc {..} =
  FieldName $ mkPrefixName <$> fromGenLocated rdrNameFieldOcc
#endif
