module HIndent.Ast.LocalBinds.Declarations
  ( LocalBindDeclarations(..)
  , mkLocalBindDeclarations
  ) where

import HIndent.Ast.WithComments (WithComments)
import HIndent.GhcOrdered.BindGroupElement (BindGroupElement)

newtype LocalBindDeclarations = LocalBindDeclarations
  { getBindGroupElements :: [WithComments BindGroupElement]
  }

mkLocalBindDeclarations ::
     [WithComments BindGroupElement] -> LocalBindDeclarations
mkLocalBindDeclarations = LocalBindDeclarations
