module HIndent.Ast.Declaration.Instance.Class
  ( ClassInstance
  , mkClassInstance
  ) where

import           HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import           HIndent.Pretty
import           HIndent.Pretty.NodeComments

newtype ClassInstance =
  ClassInstance (GHC.TyClDecl GHC.GhcPs)

instance CommentExtraction ClassInstance where
  nodeComments ClassInstance {} = NodeComments [] [] []

instance Pretty ClassInstance where
  pretty' (ClassInstance x) = pretty x

mkClassInstance :: GHC.TyClDecl GHC.GhcPs -> ClassInstance
mkClassInstance = ClassInstance
