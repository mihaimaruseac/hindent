module HIndent.Ast.Declaration.Data.Deriving.Strategy
  ( DerivingStrategy
  , mkDerivingStrategy
  , isViaStrategy
  ) where

import HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data DerivingStrategy
  = Stock
  | Anyclass
  | Newtype
  | Via (GHC.LHsSigType GHC.GhcPs)

instance CommentExtraction DerivingStrategy where
  nodeComments Stock {} = NodeComments [] [] []
  nodeComments Anyclass {} = NodeComments [] [] []
  nodeComments Newtype {} = NodeComments [] [] []
  nodeComments Via {} = NodeComments [] [] []

instance Pretty DerivingStrategy where
  pretty' Stock = string "stock"
  pretty' Anyclass = string "anyclass"
  pretty' Newtype = string "newtype"
  pretty' (Via x) = string "via " >> pretty x

mkDerivingStrategy :: GHC.DerivStrategy GHC.GhcPs -> DerivingStrategy
mkDerivingStrategy GHC.StockStrategy {} = Stock
mkDerivingStrategy GHC.AnyclassStrategy {} = Anyclass
mkDerivingStrategy GHC.NewtypeStrategy {} = Newtype
mkDerivingStrategy (GHC.ViaStrategy (GHC.XViaStrategyPs _ x)) = Via x

isViaStrategy :: DerivingStrategy -> Bool
isViaStrategy Via {} = True
isViaStrategy _ = False
