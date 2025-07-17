module HIndent.Ast.Type.ImplicitParameterName
  ( ImplicitParameterName
  , mkImplicitParameterName
  ) where

import qualified GHC.Data.FastString as GHC
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

newtype ImplicitParameterName =
  ImplicitParameterName String

instance CommentExtraction ImplicitParameterName where
  nodeComments ImplicitParameterName {} = emptyNodeComments

instance Pretty ImplicitParameterName where
  pretty' (ImplicitParameterName s) = string "?" >> string s

mkImplicitParameterName :: GHC.HsIPName -> ImplicitParameterName
mkImplicitParameterName (GHC.HsIPName fs) =
  ImplicitParameterName $ GHC.unpackFS fs
