{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.LocalBinds.ImplicitBinding
  ( ImplicitBinding
  , mkImplicitBinding
  ) where

import {-# SOURCE #-} HIndent.Ast.Expression (Expression, mkExpression)
import HIndent.Ast.NodeComments (NodeComments(..))
import HIndent.Ast.Type.ImplicitParameterName
  ( ImplicitParameterName
  , mkImplicitParameterName
  )
import HIndent.Ast.WithComments (WithComments, fromGenLocated)
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty (Pretty(..), pretty)
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data ImplicitBinding = ImplicitBinding
  { name :: WithComments ImplicitParameterName
  , expression :: WithComments Expression
  }

instance CommentExtraction ImplicitBinding where
  nodeComments _ = NodeComments [] [] []

instance Pretty ImplicitBinding where
  pretty' ImplicitBinding {..} =
    spaced [pretty name, string "=", pretty expression]

mkImplicitBinding :: GHC.IPBind GHC.GhcPs -> ImplicitBinding
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
mkImplicitBinding (GHC.IPBind _ lhs rhs) =
  ImplicitBinding
    { name = mkImplicitParameterName <$> fromGenLocated lhs
    , expression = mkExpression <$> fromGenLocated rhs
    }
#else
mkImplicitBinding (GHC.IPBind _ (Left lhs) rhs) =
  ImplicitBinding
    { name = mkImplicitParameterName <$> fromGenLocated lhs
    , expression = mkExpression <$> fromGenLocated rhs
    }
mkImplicitBinding (GHC.IPBind _ (Right _) _) =
  error "`ghc-lib-parser` never generates this AST node."
#endif
