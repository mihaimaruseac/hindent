module HIndent.Ast.Module.Declaration
  ( ModuleDeclaration
  , mkModuleDeclaration
  ) where

data ModuleDeclaration =
  ModuleDeclaration

mkModuleDeclaration :: a -> ModuleDeclaration
mkModuleDeclaration _ = ModuleDeclaration
