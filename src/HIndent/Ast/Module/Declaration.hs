module HIndent.Ast.Module.Declaration
  ( ModuleDeclaration
  , mkModuleDeclaration
  ) where

import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC

data ModuleDeclaration =
  ModuleDeclaration

mkModuleDeclaration :: GHC.HsModule' -> ModuleDeclaration
mkModuleDeclaration _ = ModuleDeclaration
