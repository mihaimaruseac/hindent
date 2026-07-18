{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Signature.StandaloneKind
  ( StandaloneKind
  , mkStandaloneKind
  ) where

import qualified GHC.Hs as GHC
import HIndent.Ast.Name.Prefix
import HIndent.Ast.Type (Type, mkTypeFromHsSigType)
import HIndent.Ast.WithComments
import HIndent.Pretty
import HIndent.Pretty.Combinators

data StandaloneKind = StandaloneKind
  { name :: WithComments PrefixName
  , kind :: WithComments Type
  }

instance Pretty StandaloneKind where
  pretty StandaloneKind {..} =
    spaced [string "type", pretty name, string "::", pretty kind]

mkStandaloneKind :: GHC.StandaloneKindSig GHC.GhcPs -> StandaloneKind
mkStandaloneKind (GHC.StandaloneKindSig _ n k) = StandaloneKind {..}
  where
    name = mkWithCommentsFromGenLocated $ fmap mkPrefixName n
    kind =
      flattenComments $ mkTypeFromHsSigType <$> mkWithCommentsFromGenLocated k
