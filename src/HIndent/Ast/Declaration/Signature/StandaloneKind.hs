{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Signature.StandaloneKind
  ( StandaloneKind
  , mkStandaloneKind
  ) where

import qualified GHC.Hs as GHC
import HIndent.Ast.Name.Prefix
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data StandaloneKind = StandaloneKind
  { name :: WithComments PrefixName
  , kind :: GHC.LHsSigType GHC.GhcPs
  }

instance CommentExtraction StandaloneKind where
  nodeComments StandaloneKind {} = NodeComments [] [] []

instance Pretty StandaloneKind where
  pretty' StandaloneKind {..} =
    spaced [string "type", pretty name, string "::", pretty kind]

mkStandaloneKind :: GHC.StandaloneKindSig GHC.GhcPs -> StandaloneKind
mkStandaloneKind (GHC.StandaloneKindSig _ n kind) = StandaloneKind {..}
  where
    name = fromGenLocated $ fmap mkPrefixName n
