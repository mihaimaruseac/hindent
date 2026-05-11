module HIndent.Ast.Type.Argument.Collection
  ( TypeArgumentCollection
  , hasTypeArguments
  , mkTypeArgumentCollection
  ) where

import Data.Maybe (mapMaybe)
import HIndent.Ast.Type.Argument
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

newtype TypeArgumentCollection =
  TypeArgumentCollection [TypeArgument]

instance CommentExtraction TypeArgumentCollection where
  nodeComments (TypeArgumentCollection arguments) =
    mconcat $ fmap nodeComments arguments

instance Pretty TypeArgumentCollection where
  pretty' (TypeArgumentCollection arguments) = spaced $ fmap pretty arguments

mkTypeArgumentCollection ::
     [GHC.HsArg GHC.GhcPs (GHC.LHsType GHC.GhcPs) (GHC.LHsType GHC.GhcPs)]
  -> TypeArgumentCollection
mkTypeArgumentCollection = TypeArgumentCollection . mapMaybe mkTypeArgument

hasTypeArguments :: TypeArgumentCollection -> Bool
hasTypeArguments (TypeArgumentCollection arguments) = not $ null arguments
