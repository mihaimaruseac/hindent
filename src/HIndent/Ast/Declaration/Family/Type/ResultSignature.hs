module HIndent.Ast.Declaration.Family.Type.ResultSignature
  ( ResultSignature(..)
  ) where

import HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.NodeComments

newtype ResultSignature =
  ResultSignature (GHC.FamilyResultSig GHC.GhcPs)

instance CommentExtraction ResultSignature where
  nodeComments (ResultSignature _) = NodeComments [] [] []

instance Pretty ResultSignature where
  pretty' (ResultSignature x) = pretty x
