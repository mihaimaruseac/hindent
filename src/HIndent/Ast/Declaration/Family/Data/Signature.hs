module HIndent.Ast.Declaration.Family.Data.Signature
  ( Signature(..)
  ) where

import           HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import           HIndent.Pretty
import           HIndent.Pretty.NodeComments

newtype Signature =
  Signature (GHC.FamilyResultSig GHC.GhcPs)

instance CommentExtraction Signature where
  nodeComments (Signature _) = NodeComments [] [] []

instance Pretty Signature where
  pretty' (Signature x) = pretty x
