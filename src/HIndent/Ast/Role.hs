{-# LANGUAGE OverloadedStrings #-}

module HIndent.Ast.Role
  ( Role
  , mkRole
  ) where

import qualified GHC.Core.TyCon as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators

data Role
  = Nominal
  | Representational
  | Phantom

instance Pretty Role where
  pretty Nominal = string "nominal"
  pretty Representational = string "representational"
  pretty Phantom = string "phantom"

mkRole :: GHC.Role -> Role
mkRole GHC.Nominal = Nominal
mkRole GHC.Representational = Representational
mkRole GHC.Phantom = Phantom
