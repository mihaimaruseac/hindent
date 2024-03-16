{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Class.FunctionalDependency
  ( FunctionalDependency
  , mkFunctionalDependency
  ) where

import HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data FunctionalDependency = FunctionalDependency
  { from :: [GHC.LIdP GHC.GhcPs]
  , to :: [GHC.LIdP GHC.GhcPs]
  }

instance CommentExtraction FunctionalDependency where
  nodeComments FunctionalDependency {} = NodeComments [] [] []

instance Pretty FunctionalDependency where
  pretty' (FunctionalDependency {..}) =
    spaced $ fmap pretty from ++ [string "->"] ++ fmap pretty to

mkFunctionalDependency :: GHC.FunDep GHC.GhcPs -> FunctionalDependency
mkFunctionalDependency (GHC.FunDep _ from to) = FunctionalDependency {..}
