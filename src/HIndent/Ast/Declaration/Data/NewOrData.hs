module HIndent.Ast.Declaration.Data.NewOrData
  ( NewOrData
  , mkNewOrData
  ) where

import HIndent.Ast.NodeComments
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import qualified Language.Haskell.Syntax.Decls as GHC

data NewOrData
  = Newtype
  | Data

instance CommentExtraction NewOrData where
  nodeComments Newtype = NodeComments [] [] []
  nodeComments Data = NodeComments [] [] []

instance Pretty NewOrData where
  pretty' Newtype = string "newtype"
  pretty' Data = string "data"

mkNewOrData :: GHC.NewOrData -> NewOrData
mkNewOrData GHC.DataType = Data
mkNewOrData GHC.NewType = Newtype
