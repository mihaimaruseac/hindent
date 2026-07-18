module HIndent.Ast.Type.Argument.Collection
  ( TypeArgumentCollection
  , hasTypeArguments
  , mkTypeArgumentCollection
  ) where

import Data.Maybe (mapMaybe)
import HIndent.Ast.Type.Argument
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators

newtype TypeArgumentCollection =
  TypeArgumentCollection [TypeArgument]

instance Pretty TypeArgumentCollection where
  pretty (TypeArgumentCollection arguments) = spaced $ fmap pretty arguments

mkTypeArgumentCollection ::
     [GHC.HsArg GHC.GhcPs (GHC.LHsType GHC.GhcPs) (GHC.LHsType GHC.GhcPs)]
  -> TypeArgumentCollection
mkTypeArgumentCollection = TypeArgumentCollection . mapMaybe mkTypeArgument

hasTypeArguments :: TypeArgumentCollection -> Bool
hasTypeArguments (TypeArgumentCollection arguments) = not $ null arguments
