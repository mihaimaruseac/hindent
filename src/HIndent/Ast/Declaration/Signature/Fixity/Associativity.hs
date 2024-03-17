module HIndent.Ast.Declaration.Signature.Fixity.Associativity
  ( Associativity
  , mkAssociativity
  ) where

import qualified GHC.Types.Fixity as GHC
import HIndent.Ast.NodeComments
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data Associativity
  = LeftAssoc
  | RightAssoc
  | None

instance CommentExtraction Associativity where
  nodeComments LeftAssoc = NodeComments [] [] []
  nodeComments RightAssoc = NodeComments [] [] []
  nodeComments None = NodeComments [] [] []

instance Pretty Associativity where
  pretty' LeftAssoc = string "infixl"
  pretty' RightAssoc = string "infixr"
  pretty' None = string "infix"

mkAssociativity :: GHC.FixityDirection -> Associativity
mkAssociativity GHC.InfixL = LeftAssoc
mkAssociativity GHC.InfixR = RightAssoc
mkAssociativity GHC.InfixN = None
