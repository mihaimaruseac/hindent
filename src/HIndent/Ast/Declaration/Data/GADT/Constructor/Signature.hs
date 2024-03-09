{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Data.GADT.Constructor.Signature
  ( ConstructorSignature(..)
  , mkConstructorSignature
  , prettyHorizontally
  , prettyVertically
  ) where

import HIndent.Ast.NodeComments
import HIndent.Ast.Type
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Printer

data ConstructorSignature
  = ByArrows
      { parameters :: [WithComments Type]
      , result :: WithComments Type
      }
  | Record
      { fields :: WithComments [GHC.LConDeclField GHC.GhcPs]
      , result :: WithComments Type
      }

instance CommentExtraction ConstructorSignature where
  nodeComments (ByArrows {}) = NodeComments [] [] []
  nodeComments (Record {}) = NodeComments [] [] []

prettyHorizontally :: ConstructorSignature -> Printer ()
prettyHorizontally (ByArrows {..}) =
  inter (string " -> ") $ fmap pretty parameters ++ [pretty result]
prettyHorizontally (Record {..}) =
  inter
    (string " -> ")
    [prettyWith fields (vFields' . fmap pretty), pretty result]

prettyVertically :: ConstructorSignature -> Printer ()
prettyVertically (ByArrows {..}) =
  prefixedLined "-> " $ fmap pretty parameters ++ [pretty result]
prettyVertically (Record {..}) =
  prefixedLined
    "-> "
    [prettyWith fields (vFields' . fmap pretty), pretty result]

mkConstructorSignature :: GHC.ConDecl GHC.GhcPs -> Maybe ConstructorSignature
mkConstructorSignature GHC.ConDeclGADT {con_g_args = GHC.PrefixConGADT xs, ..} =
  Just
    $ ByArrows
        { parameters =
            fmap (fmap mkType . fromGenLocated . GHC.hsScaledThing) xs
        , result = mkType <$> fromGenLocated con_res_ty
        }
#if MIN_VERSION_ghc_lib_parser(9, 4, 0)
mkConstructorSignature GHC.ConDeclGADT {con_g_args = GHC.RecConGADT xs _, ..} =
  Just
    $ Record
        { fields = fromGenLocated xs
        , result = mkType <$> fromGenLocated con_res_ty
        }
#else
mkConstructorSignature GHC.ConDeclGADT {con_g_args = GHC.RecConGADT xs, ..} =
  Just
    $ Record
        { fields = fromGenLocated xs
        , result = mkType <$> fromGenLocated con_res_ty
        }
#endif
mkConstructorSignature GHC.ConDeclH98 {} = Nothing
