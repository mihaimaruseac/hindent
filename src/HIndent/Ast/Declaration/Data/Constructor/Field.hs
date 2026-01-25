{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Data.Constructor.Field
  ( ConstructorField
  , mkConstructorField
  ) where

import HIndent.Ast.NodeComments
import HIndent.Ast.Type
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.NodeComments

newtype ConstructorField = ConstructorField
  { ty :: WithComments Type
  }

instance CommentExtraction ConstructorField where
  nodeComments ConstructorField {} = NodeComments [] [] []

instance Pretty ConstructorField where
  pretty' ConstructorField {..} = pretty ty
#if MIN_VERSION_ghc_lib_parser(9, 14, 0)
mkConstructorField :: GHC.HsConDeclField GHC.GhcPs -> ConstructorField
mkConstructorField field = ConstructorField {ty = mkTypeFromConDeclField field}
#else
mkConstructorField ::
     GHC.HsScaled GHC.GhcPs (GHC.LBangType GHC.GhcPs) -> ConstructorField
mkConstructorField (GHC.HsScaled _ bangTy) =
  ConstructorField {ty = mkType <$> fromGenLocated bangTy}
#endif
