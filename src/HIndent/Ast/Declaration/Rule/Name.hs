module HIndent.Ast.Declaration.Rule.Name
  ( RuleName
  , mkRuleName
  ) where

import qualified GHC.Data.FastString as GHC
import qualified GHC.Types.Basic as GHC
import HIndent.Ast.TextValue (TextValue, mkTextValueFromString)
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators

newtype RuleName =
  RuleName TextValue
  deriving (Eq, Show)

instance Pretty RuleName where
  pretty (RuleName name) = doubleQuotes $ pretty name

mkRuleName :: GHC.RuleName -> RuleName
mkRuleName = RuleName . mkTextValueFromString . GHC.unpackFS
