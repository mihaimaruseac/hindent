{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Expression.FieldSelector
  ( FieldSelector
  , mkFieldSelector
  ) where

import qualified GHC.Data.FastString as GHC
import qualified GHC.Hs as GHC
import HIndent.Ast.Name.Prefix (PrefixName, fromString)
import HIndent.Ast.NodeComments (NodeComments(..))
import HIndent.Ast.WithComments (WithComments, fromGenLocated)
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty)
import HIndent.Pretty.NodeComments (CommentExtraction(..))
import qualified Language.Haskell.Syntax.Basic as GHC

newtype FieldSelector = FieldSelector
  { name :: WithComments PrefixName
  }

instance CommentExtraction FieldSelector where
  nodeComments FieldSelector {} = NodeComments [] [] []

instance Pretty FieldSelector where
  pretty' FieldSelector {..} = pretty name

mkFieldSelector :: GHC.DotFieldOcc GHC.GhcPs -> FieldSelector
mkFieldSelector GHC.DotFieldOcc {..} =
  FieldSelector
    { name =
        fmap
          (fromString . GHC.unpackFS . GHC.field_label)
          (fromGenLocated dfoLabel)
    }
