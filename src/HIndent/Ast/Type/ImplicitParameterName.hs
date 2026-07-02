{-# LANGUAGE OverloadedStrings #-}

module HIndent.Ast.Type.ImplicitParameterName
  ( ImplicitParameterName
  , mkImplicitParameterName
  ) where

import qualified GHC.Data.FastString as GHC
import HIndent.Ast.TextValue (TextValue, mkTextValueFromString)
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

newtype ImplicitParameterName =
  ImplicitParameterName TextValue

instance CommentExtraction ImplicitParameterName where
  nodeComments ImplicitParameterName {} = emptyNodeComments

instance Pretty ImplicitParameterName where
  pretty (ImplicitParameterName name) = string "?" >> pretty name

mkImplicitParameterName :: GHC.HsIPName -> ImplicitParameterName
mkImplicitParameterName (GHC.HsIPName fs) =
  ImplicitParameterName $ mkTextValueFromString $ GHC.unpackFS fs
