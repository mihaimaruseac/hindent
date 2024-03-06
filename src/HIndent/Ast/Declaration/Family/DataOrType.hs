module HIndent.Ast.Declaration.Family.DataOrType
  ( DataOrType
  , mkDataOrType
  ) where

import           HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments

data DataOrType
  = Data
  | Type

instance CommentExtraction DataOrType where
  nodeComments Data = NodeComments [] [] []
  nodeComments Type = NodeComments [] [] []

instance Pretty DataOrType where
  pretty' Data = string "data"
  pretty' Type = string "type"

mkDataOrType :: GHC.FamilyInfo GHC.GhcPs -> DataOrType
mkDataOrType GHC.DataFamily           = Data
mkDataOrType GHC.OpenTypeFamily       = Type
mkDataOrType (GHC.ClosedTypeFamily _) = Type
