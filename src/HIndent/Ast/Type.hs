module HIndent.Ast.Type
  ( Type
  , mkType
  ) where

import HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.NodeComments

newtype Type =
  Type (GHC.HsType GHC.GhcPs)

instance CommentExtraction Type where
  nodeComments (Type _) = NodeComments [] [] []

instance Pretty Type where
  pretty' (Type x) = pretty x

mkType :: GHC.HsType GHC.GhcPs -> Type
mkType = Type
