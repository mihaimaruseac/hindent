module HIndent.Ast.Declaration.Collection
  ( DeclarationCollection
  , mkDeclarationCollection
  ) where

data DeclarationCollection =
  DeclarationCollection

mkDeclarationCollection :: a -> DeclarationCollection
mkDeclarationCollection _ = DeclarationCollection
