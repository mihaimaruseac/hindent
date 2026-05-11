{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Type.Argument
  ( TypeArgument
  , mkTypeArgument
  ) where

import HIndent.Ast.Type (Type, mkType)
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data TypeArgument
  = TypeArgument
      { argType :: WithComments Type
      }
  | KindArgument
      { argKind :: WithComments Type
      }

instance CommentExtraction TypeArgument where
  nodeComments TypeArgument {..} = nodeComments argType
  nodeComments KindArgument {..} = nodeComments argKind

instance Pretty TypeArgument where
  pretty' TypeArgument {..} = pretty argType
  pretty' KindArgument {..} = string "@" >> pretty argKind

mkTypeArgument ::
     GHC.HsArg GHC.GhcPs (GHC.LHsType GHC.GhcPs) (GHC.LHsType GHC.GhcPs)
  -> Maybe TypeArgument
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkTypeArgument (GHC.HsValArg _ argType) =
  Just TypeArgument {argType = mkType <$> fromGenLocated argType}
mkTypeArgument (GHC.HsTypeArg _ argKind) =
  Just KindArgument {argKind = mkType <$> fromGenLocated argKind}
mkTypeArgument GHC.HsArgPar {} = Nothing
#else
mkTypeArgument (GHC.HsValArg argType) =
  Just TypeArgument {argType = mkType <$> fromGenLocated argType}
mkTypeArgument (GHC.HsTypeArg _ argKind) =
  Just KindArgument {argKind = mkType <$> fromGenLocated argKind}
mkTypeArgument GHC.HsArgPar {} = Nothing
#endif
