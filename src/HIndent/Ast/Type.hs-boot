{-# LANGUAGE CPP #-}

module HIndent.Ast.Type where

import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Ast.WithComments (WithComments)
import HIndent.Pretty.NodeComments
import {-# SOURCE #-} HIndent.Pretty

data Type

instance CommentExtraction Type
instance Pretty Type

mkType :: GHC.HsType GHC.GhcPs -> Type
#if MIN_VERSION_ghc_lib_parser(9, 14, 0)
mkTypeFromConDeclField :: GHC.HsConDeclField GHC.GhcPs -> WithComments Type
#else
mkTypeFromConDeclField :: GHC.ConDeclField GHC.GhcPs -> WithComments Type
#endif
