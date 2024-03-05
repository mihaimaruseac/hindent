module HIndent.Ast.Import.ImportingOrHiding
  ( ImportingOrHiding(..)
  ) where

data ImportingOrHiding
  = Importing
  | Hiding
  deriving (Eq)
