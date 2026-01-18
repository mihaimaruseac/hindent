module HIndent.Ast.LocalBinds.Declarations
  ( LocalBindDeclarations(..)
  , mkLocalBindDeclarations
  ) where

import HIndent.Ast.GhcOrdered.BindGroupElement (BindGroupElements)

newtype LocalBindDeclarations = LocalBindDeclarations
  { getBindGroupElements :: BindGroupElements
  }

mkLocalBindDeclarations :: BindGroupElements -> LocalBindDeclarations
mkLocalBindDeclarations = LocalBindDeclarations
