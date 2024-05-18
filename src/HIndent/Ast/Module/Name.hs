module HIndent.Ast.Module.Name
  ( ModuleName
  , mkModuleName
  ) where

import qualified GHC.Unit as GHC
import HIndent.Ast.NodeComments
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

newtype ModuleName =
  ModuleName String

instance CommentExtraction ModuleName where
  nodeComments _ = NodeComments [] [] []

instance Pretty ModuleName where
  pretty' (ModuleName x) = string "module " >> string x

mkModuleName :: GHC.ModuleName -> ModuleName
mkModuleName = ModuleName . GHC.moduleNameString
