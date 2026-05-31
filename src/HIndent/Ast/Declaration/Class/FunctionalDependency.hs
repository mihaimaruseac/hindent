{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Class.FunctionalDependency
  ( FunctionalDependency
  , mkFunctionalDependency
  ) where

import HIndent.Ast.Name.Prefix
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data FunctionalDependency = FunctionalDependency
  { from :: [WithComments PrefixName]
  , to :: [WithComments PrefixName]
  }

instance CommentExtraction FunctionalDependency where
  nodeComments FunctionalDependency {} = NodeComments [] [] []

instance Pretty FunctionalDependency where
  pretty' (FunctionalDependency {..}) =
    spaced $ fmap pretty from ++ [string "->"] ++ fmap pretty to

mkFunctionalDependency :: GHC.FunDep GHC.GhcPs -> FunctionalDependency
mkFunctionalDependency (GHC.FunDep _ f t) = FunctionalDependency {..}
  where
    from = fmap (mkWithCommentsFromGenLocated . fmap mkPrefixName) f
    to = fmap (mkWithCommentsFromGenLocated . fmap mkPrefixName) t
