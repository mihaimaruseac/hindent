module HIndent.Ast.LocalBinds.Declarations
  ( LocalBindDeclarations(..)
  , mkLocalBindDeclarations
  ) where

import HIndent.Ast.GhcOrdered.BindGroupElement (BindGroupElement)
import HIndent.Ast.WithComments (WithComments)

newtype LocalBindDeclarations = LocalBindDeclarations
  { getBindGroupElements :: [WithComments BindGroupElement]
  }

mkLocalBindDeclarations ::
     [WithComments BindGroupElement] -> LocalBindDeclarations
mkLocalBindDeclarations = LocalBindDeclarations
