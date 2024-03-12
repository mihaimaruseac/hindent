{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Data.GADT.Constructor.Signature
  ( ConstructorSignature(..)
  , mkConstructorSignature
  ) where

import HIndent.Ast.NodeComments
import HIndent.Ast.Type
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty.NodeComments

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
