{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module HIndent.Ast.Type.Unpackedness
  ( Unpackedness
  , mkUnpackedness
  ) where

import qualified GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators

data Unpackedness
  = Unpack
  | NoUnpack
  deriving (Eq)

instance Pretty Unpackedness where
  pretty Unpack = string "{-# UNPACK #-}"
  pretty NoUnpack = string "{-# NOUNPACK #-}"

mkUnpackedness :: GHC.SrcUnpackedness -> Maybe Unpackedness
mkUnpackedness GHC.SrcUnpack = Just Unpack
mkUnpackedness GHC.SrcNoUnpack = Just NoUnpack
mkUnpackedness GHC.NoSrcUnpack = Nothing
