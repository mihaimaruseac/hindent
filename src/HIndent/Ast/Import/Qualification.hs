module HIndent.Ast.Import.Qualification
  ( Qualification(..)
  ) where

data Qualification
  = NotQualified
  | FullyQualified
  | QualifiedAs String
  deriving (Eq)
