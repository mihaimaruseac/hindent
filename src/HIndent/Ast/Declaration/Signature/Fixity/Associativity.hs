{-# LANGUAGE OverloadedStrings #-}

module HIndent.Ast.Declaration.Signature.Fixity.Associativity
  ( Associativity
  , mkAssociativity
  ) where

import qualified GHC.Types.Fixity as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators

data Associativity
  = LeftAssoc
  | RightAssoc
  | None

instance Pretty Associativity where
  pretty LeftAssoc = string "infixl"
  pretty RightAssoc = string "infixr"
  pretty None = string "infix"

mkAssociativity :: GHC.FixityDirection -> Associativity
mkAssociativity GHC.InfixL = LeftAssoc
mkAssociativity GHC.InfixR = RightAssoc
mkAssociativity GHC.InfixN = None
