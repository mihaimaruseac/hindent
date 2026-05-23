module HIndent.Ast.Module.Name
  ( ModuleName
  , mkModuleName
  ) where

import qualified GHC.Unit as GHC
import HIndent.Ast.NodeComments
import HIndent.Ast.TextValue
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.NodeComments

newtype ModuleName =
  ModuleName TextValue
  deriving (Eq, Ord)

instance CommentExtraction ModuleName where
  nodeComments _ = NodeComments [] [] []

instance Pretty ModuleName where
  pretty' (ModuleName value) = pretty value

mkModuleName :: GHC.ModuleName -> ModuleName
mkModuleName = ModuleName . mkTextValueFromString . GHC.moduleNameString
