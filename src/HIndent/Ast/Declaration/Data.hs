module HIndent.Ast.Declaration.Data
  ( Data
  , mkData
  ) where

import           HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import           HIndent.Pretty
import           HIndent.Pretty.NodeComments

newtype Data =
  Data (GHC.TyClDecl GHC.GhcPs)

instance CommentExtraction Data where
  nodeComments (Data _) = NodeComments [] [] []

instance Pretty Data where
  pretty' (Data x) = pretty x

mkData :: GHC.TyClDecl GHC.GhcPs -> Data
mkData = Data
