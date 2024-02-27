module HIndent.Ast.Module.Name
  ( ModuleName
  , mkModuleName
  ) where

import qualified GHC.Unit as GHC
import HIndent.Pretty.Combinators

newtype ModuleName =
  ModuleName String

mkModuleName :: GHC.ModuleName -> ModuleName
mkModuleName = ModuleName . showOutputable
