{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Expression.FieldSelector
  ( FieldSelector
  , mkFieldSelector
  ) where

import qualified GHC.Data.FastString as FastString
import qualified GHC.Hs as GHC
import qualified GHC.Types.SrcLoc as Src
import HIndent.Ast.Name.Prefix (PrefixName, fromString)
import HIndent.Ast.NodeComments (NodeComments(..))
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty)
import HIndent.Pretty.NodeComments (CommentExtraction(..))
import qualified Language.Haskell.Syntax.Basic as HS

newtype FieldSelector = FieldSelector
  { name :: PrefixName
  }

instance CommentExtraction FieldSelector where
  nodeComments FieldSelector {} = NodeComments [] [] []

instance Pretty FieldSelector where
  pretty' FieldSelector {..} = pretty name

mkFieldSelector :: GHC.DotFieldOcc GHC.GhcPs -> FieldSelector
mkFieldSelector GHC.DotFieldOcc {..} =
  FieldSelector
    { name =
        fromString $ FastString.unpackFS $ HS.field_label $ Src.unLoc dfoLabel
    }
