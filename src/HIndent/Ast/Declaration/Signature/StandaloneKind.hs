{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Signature.StandaloneKind
  ( StandaloneKind
  , mkStandaloneKind
  ) where

import qualified GHC.Hs as GHC
import HIndent.Ast.NodeComments
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data StandaloneKind = StandaloneKind
  { name :: GHC.LIdP GHC.GhcPs
  , kind :: GHC.LHsSigType GHC.GhcPs
  }

instance CommentExtraction StandaloneKind where
  nodeComments StandaloneKind {} = NodeComments [] [] []

instance Pretty StandaloneKind where
  pretty' StandaloneKind {..} =
    spaced [string "type", pretty name, string "::", pretty kind]

mkStandaloneKind :: GHC.StandaloneKindSig GHC.GhcPs -> StandaloneKind
mkStandaloneKind (GHC.StandaloneKindSig _ name kind) = StandaloneKind {..}
