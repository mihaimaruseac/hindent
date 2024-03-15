module HIndent.Ast.Declaration.Family.Type.Injectivity
  ( Injectivity
  , mkInjectivity
  ) where

import HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.NodeComments

newtype Injectivity =
  Injectivity (GHC.InjectivityAnn GHC.GhcPs)

instance CommentExtraction Injectivity where
  nodeComments (Injectivity _) = NodeComments [] [] []

instance Pretty Injectivity where
  pretty' (Injectivity x) = pretty x

mkInjectivity :: GHC.InjectivityAnn GHC.GhcPs -> Injectivity
mkInjectivity = Injectivity
