module HIndent.Ast.LocalBinds.Declaration
  ( LocalDeclaration
  , mkLocalSignatureDeclaration
  , mkLocalBindingDeclaration
  ) where

import {-# SOURCE #-} HIndent.Ast.Declaration.Bind
import HIndent.Ast.Declaration.Signature
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty (Pretty(..), pretty)

data LocalDeclaration
  = Binding Bind
  | Signature Signature

instance Pretty LocalDeclaration where
  pretty (Binding bind) = pretty bind
  pretty (Signature signature) = pretty signature

mkLocalSignatureDeclaration :: GHC.Sig GHC.GhcPs -> LocalDeclaration
mkLocalSignatureDeclaration = Signature . mkSignature

mkLocalBindingDeclaration :: GHC.HsBind GHC.GhcPs -> LocalDeclaration
mkLocalBindingDeclaration = Binding . mkBind
