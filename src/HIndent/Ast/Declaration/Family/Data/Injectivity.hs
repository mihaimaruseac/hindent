module HIndent.Ast.Declaration.Family.Data.Injectivity
  ( Injectivity(..)
  ) where

import HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.NodeComments

newtype Injectivity =
  Injectivity (GHC.InjectivityAnn GHC.GhcPs)

instance CommentExtraction Injectivity where
  nodeComments (Injectivity _) = NodeComments [] [] []

instance Pretty Injectivity where
  pretty' (Injectivity x) = pretty x
