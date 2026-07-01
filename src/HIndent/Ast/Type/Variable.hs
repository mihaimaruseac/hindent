{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Type.Variable
  ( TypeVariable
  , mkTypeVariable
  ) where

import qualified GHC.Hs as GHC
import HIndent.Ast.Name.Prefix
import {-# SOURCE #-} HIndent.Ast.Type
import HIndent.Ast.WithComments
import HIndent.Pretty
import HIndent.Pretty.Combinators
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
import qualified HIndent.Ast.Name.Prefix as Prefix
#endif
data TypeVariable = TypeVariable
  { name :: WithComments PrefixName
  , kind :: Maybe (WithComments Type)
  }

instance Pretty TypeVariable where
  pretty TypeVariable {kind = Just kind, ..} =
    parens $ pretty name >> string " :: " >> pretty kind
  pretty TypeVariable {kind = Nothing, ..} = pretty name

mkTypeVariable :: GHC.HsTyVarBndr a GHC.GhcPs -> TypeVariable
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkTypeVariable GHC.HsTvb {..} = TypeVariable {..}
  where
    name =
      case tvb_var of
        GHC.HsBndrVar _ n -> mkWithCommentsFromGenLocated $ fmap mkPrefixName n
        GHC.HsBndrWildCard _ -> mkWithComments (Prefix.fromString "_")
    kind =
      case tvb_kind of
        GHC.HsBndrKind _ k -> Just $ mkType <$> mkWithCommentsFromGenLocated k
        GHC.HsBndrNoKind _ -> Nothing
#else
mkTypeVariable (GHC.UserTyVar _ _ n) = TypeVariable {..}
  where
    name = mkWithCommentsFromGenLocated $ fmap mkPrefixName n
    kind = Nothing
mkTypeVariable (GHC.KindedTyVar _ _ n k) = TypeVariable {..}
  where
    name = mkWithCommentsFromGenLocated $ fmap mkPrefixName n
    kind = Just $ mkType <$> mkWithCommentsFromGenLocated k
#endif
