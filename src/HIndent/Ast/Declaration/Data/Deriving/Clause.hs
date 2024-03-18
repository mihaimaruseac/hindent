module HIndent.Ast.Declaration.Data.Deriving.Clause
  ( DerivingClause
  , mkDerivingClause
  , hasDerivings
  ) where

import HIndent.Ast.Declaration.Data.Deriving
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

newtype DerivingClause =
  DerivingClause [WithComments Deriving]

instance CommentExtraction DerivingClause where
  nodeComments DerivingClause {} = NodeComments [] [] []

instance Pretty DerivingClause where
  pretty' (DerivingClause xs) = lined $ fmap pretty xs

mkDerivingClause :: GHC.HsDeriving GHC.GhcPs -> DerivingClause
mkDerivingClause = DerivingClause . fmap (fmap mkDeriving . fromGenLocated)

hasDerivings :: DerivingClause -> Bool
hasDerivings (DerivingClause []) = False
hasDerivings _ = True
