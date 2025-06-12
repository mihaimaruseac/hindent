{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Signature.Fixity
  ( Fixity
  , mkFixity
  ) where

import qualified GHC.Types.Fixity as GHC
import HIndent.Ast.Declaration.Signature.Fixity.Associativity
import HIndent.Ast.NodeComments
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data Fixity = Fixity
  { level :: Int
  , associativity :: Associativity
  }

instance CommentExtraction Fixity where
  nodeComments Fixity {} = NodeComments [] [] []

instance Pretty Fixity where
  pretty' Fixity {..} = spaced [pretty associativity, string $ show level]

mkFixity :: GHC.Fixity -> Fixity
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkFixity (GHC.Fixity level associativity) =
  Fixity level (mkAssociativity associativity)
#else
mkFixity (GHC.Fixity _ level associativity) =
  Fixity level (mkAssociativity associativity)
#endif
