module HIndent.Ast.Cmd
  ( Cmd
  , mkCmd
  ) where

import qualified GHC.Hs as GHC

data Cmd

mkCmd :: GHC.HsCmd GHC.GhcPs -> Cmd
