module HIndent.Ast.FileHeaderPragma
  ( FileHeaderPragma
  , mkFileHeaderPragma
  ) where

import HIndent.Ast.NodeComments
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

newtype FileHeaderPragma =
  FileHeaderPragma String

instance CommentExtraction FileHeaderPragma where
  nodeComments _ = NodeComments [] [] []

instance Pretty FileHeaderPragma where
  pretty' (FileHeaderPragma x) = string x

mkFileHeaderPragma :: String -> FileHeaderPragma
mkFileHeaderPragma = FileHeaderPragma
