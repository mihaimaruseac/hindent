module HIndent.Ast.Declaration.Splice
  ( SpliceDeclaration
  , mkSpliceDeclaration
  ) where

import HIndent.Ast.Expression.Splice
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.NodeComments

newtype SpliceDeclaration =
  SpliceDeclaration (WithComments Splice)

instance CommentExtraction SpliceDeclaration where
  nodeComments SpliceDeclaration {} = NodeComments [] [] []

instance Pretty SpliceDeclaration where
  pretty' (SpliceDeclaration splice) = pretty splice

mkSpliceDeclaration :: GHC.SpliceDecl GHC.GhcPs -> SpliceDeclaration
mkSpliceDeclaration (GHC.SpliceDecl _ sp _) =
  SpliceDeclaration $ mkSplice <$> fromGenLocated sp
