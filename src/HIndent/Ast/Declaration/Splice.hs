module HIndent.Ast.Declaration.Splice
  ( SpliceDeclaration
  , mkSpliceDeclaration
  ) where

import HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.NodeComments

newtype SpliceDeclaration =
  SpliceDeclaration (GHC.SpliceDecl GHC.GhcPs)

instance CommentExtraction SpliceDeclaration where
  nodeComments SpliceDeclaration {} = NodeComments [] [] []

instance Pretty SpliceDeclaration where
  pretty' (SpliceDeclaration (GHC.SpliceDecl _ sp _)) = pretty sp

mkSpliceDeclaration :: GHC.SpliceDecl GHC.GhcPs -> SpliceDeclaration
mkSpliceDeclaration = SpliceDeclaration
