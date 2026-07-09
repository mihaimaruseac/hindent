module HIndent.Ast.Declaration.Data.Deriving.Clause
  ( DerivingClause
  , mkDerivingClause
  , hasDerivings
  ) where

import HIndent.Ast.Declaration.Data.Deriving
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators

newtype DerivingClause =
  DerivingClause [WithComments Deriving]

instance Pretty DerivingClause where
  pretty (DerivingClause xs) = lined $ fmap pretty xs

mkDerivingClause :: GHC.HsDeriving GHC.GhcPs -> DerivingClause
mkDerivingClause =
  DerivingClause . fmap (fmap mkDeriving . mkWithCommentsFromGenLocated)

hasDerivings :: DerivingClause -> Bool
hasDerivings (DerivingClause []) = False
hasDerivings _ = True
