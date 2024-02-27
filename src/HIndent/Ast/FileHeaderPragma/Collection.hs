module HIndent.Ast.FileHeaderPragma.Collection
  ( FileHeaderPragmaCollection
  , mkFileHeaderPragmaCollection
  ) where

import HIndent.Ast.FileHeaderPragma

newtype FileHeaderPragmaCollection =
  FileHeaderPragmaCollection [FileHeaderPragma]

mkFileHeaderPragmaCollection :: a -> FileHeaderPragmaCollection
mkFileHeaderPragmaCollection _ = FileHeaderPragmaCollection []
