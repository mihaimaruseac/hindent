module HIndent.Ast.Module.Name
  ( ModuleName
  , mkModuleName
  ) where

import qualified GHC.Unit as GHC
import HIndent.Ast.TextValue
import HIndent.Pretty

newtype ModuleName =
  ModuleName TextValue
  deriving (Eq, Ord)

instance Pretty ModuleName where
  pretty (ModuleName value) = pretty value

mkModuleName :: GHC.ModuleName -> ModuleName
mkModuleName = ModuleName . mkTextValueFromString . GHC.moduleNameString
