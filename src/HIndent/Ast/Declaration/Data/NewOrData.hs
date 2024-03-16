{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Data.NewOrData
  ( NewOrData
  , mkNewOrData
  ) where

import HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data NewOrData
  = Newtype
  | Data

instance CommentExtraction NewOrData where
  nodeComments Newtype = NodeComments [] [] []
  nodeComments Data = NodeComments [] [] []

instance Pretty NewOrData where
  pretty' Newtype = string "newtype"
  pretty' Data = string "data"

mkNewOrData :: GHC.HsDataDefn GHC.GhcPs -> NewOrData
#if MIN_VERSION_ghc_lib_parser(9, 6, 0)
mkNewOrData GHC.HsDataDefn {..}
  | GHC.NewTypeCon _ <- dd_cons = Newtype
  | GHC.DataTypeCons _ _ <- dd_cons = Data
#else
mkNewOrData GHC.HsDataDefn {..}
  | GHC.NewType <- dd_ND = Newtype
  | GHC.DataType <- dd_ND = Data
#endif
