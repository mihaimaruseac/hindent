module HIndent.Ast.Declaration.Family.Type.ResultSignature
  ( ResultSignature(..)
  , mkResultSignature
  ) where

import HIndent.Ast.NodeComments
import HIndent.Ast.Type
import HIndent.Ast.Type.Variable
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data ResultSignature
  = NoSig
  | Kind (GHC.LHsKind GHC.GhcPs)
  | TypeVariable (WithComments TypeVariable)

instance CommentExtraction ResultSignature where
  nodeComments NoSig = NodeComments [] [] []
  nodeComments Kind {} = NodeComments [] [] []
  nodeComments TypeVariable {} = NodeComments [] [] []

instance Pretty ResultSignature where
  pretty' NoSig = return ()
  pretty' (Kind x) = string " :: " >> pretty (fmap mkType x)
  pretty' (TypeVariable x) = string " = " >> pretty x

mkResultSignature :: GHC.FamilyResultSig GHC.GhcPs -> ResultSignature
mkResultSignature (GHC.NoSig _) = NoSig
mkResultSignature (GHC.KindSig _ x) = Kind x
mkResultSignature (GHC.TyVarSig _ x) = TypeVariable var
  where
    var = mkTypeVariable <$> fromGenLocated x
