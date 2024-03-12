module HIndent.Ast.Declaration.Family.Type.Injectivity
  ( Injectivity(..)
  ) where

import HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty.NodeComments

newtype Injectivity =
  Injectivity (GHC.InjectivityAnn GHC.GhcPs)

instance CommentExtraction Injectivity where
  nodeComments (Injectivity _) = NodeComments [] [] []
