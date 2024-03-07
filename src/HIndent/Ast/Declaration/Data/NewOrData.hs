module HIndent.Ast.Declaration.Data.NewOrData
  ( NewOrData
  , mkNewOrData
  ) where

import qualified Language.Haskell.Syntax.Decls as GHC

data NewOrData
  = Newtype
  | Data

mkNewOrData :: GHC.NewOrData -> NewOrData
mkNewOrData GHC.DataType = Data
mkNewOrData GHC.NewType  = Newtype
