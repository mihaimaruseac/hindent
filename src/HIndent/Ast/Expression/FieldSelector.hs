{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Expression.FieldSelector
  ( FieldSelector
  , mkFieldSelector
  ) where

import qualified GHC.Data.FastString as GHC
import qualified GHC.Hs as GHC
import HIndent.Ast.Name.Prefix (PrefixName, fromString)
import HIndent.Ast.WithComments (WithComments, mkWithCommentsFromGenLocated)
import HIndent.Pretty (Pretty(..))
import qualified Language.Haskell.Syntax.Basic as GHC

newtype FieldSelector = FieldSelector
  { name :: WithComments PrefixName
  }

instance Pretty FieldSelector where
  pretty FieldSelector {..} = pretty name

mkFieldSelector :: GHC.DotFieldOcc GHC.GhcPs -> FieldSelector
mkFieldSelector GHC.DotFieldOcc {..} =
  FieldSelector
    { name =
        fmap
          (fromString . GHC.unpackFS . GHC.field_label)
          (mkWithCommentsFromGenLocated dfoLabel)
    }
