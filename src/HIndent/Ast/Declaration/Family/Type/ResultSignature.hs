{-# LANGUAGE OverloadedStrings #-}

module HIndent.Ast.Declaration.Family.Type.ResultSignature
  ( ResultSignature(..)
  , mkResultSignature
  ) where

import HIndent.Ast.Type
import HIndent.Ast.Type.Variable
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators

data ResultSignature
  = NoSig
  | Kind (WithComments Type)
  | TypeVariable (WithComments TypeVariable)

instance Pretty ResultSignature where
  pretty NoSig = return ()
  pretty (Kind x) = string " :: " >> pretty x
  pretty (TypeVariable x) = string " = " >> pretty x

mkResultSignature :: GHC.FamilyResultSig GHC.GhcPs -> ResultSignature
mkResultSignature (GHC.NoSig _) = NoSig
mkResultSignature (GHC.KindSig _ x) =
  Kind $ mkType <$> mkWithCommentsFromGenLocated x
mkResultSignature (GHC.TyVarSig _ x) = TypeVariable var
  where
    var = mkTypeVariable <$> mkWithCommentsFromGenLocated x
