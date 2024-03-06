module HIndent.Ast.Declaration
  ( Declaration(..)
  , mkDeclaration
  ) where

import           HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import           HIndent.Pretty
import           HIndent.Pretty.NodeComments

newtype Declaration =
  Declaration (GHC.HsDecl GHC.GhcPs)

instance CommentExtraction Declaration where
  nodeComments (Declaration _) = NodeComments [] [] []

instance Pretty Declaration where
  pretty' (Declaration decl) = pretty decl

mkDeclaration :: GHC.HsDecl GHC.GhcPs -> Declaration
mkDeclaration = Declaration
