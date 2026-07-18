{-# LANGUAGE OverloadedStrings #-}

module HIndent.Ast.Declaration.Data.Deriving.Strategy
  ( DerivingStrategy
  , mkDerivingStrategy
  , isViaStrategy
  ) where

import HIndent.Ast.Type (Type, mkTypeFromHsSigType)
import HIndent.Ast.WithComments
  ( WithComments
  , flattenComments
  , mkWithCommentsFromGenLocated
  )
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators

data DerivingStrategy
  = Stock
  | Anyclass
  | Newtype
  | Via (WithComments Type)

instance Pretty DerivingStrategy where
  pretty Stock = string "stock"
  pretty Anyclass = string "anyclass"
  pretty Newtype = string "newtype"
  pretty (Via x) = string "via " >> pretty x

mkDerivingStrategy :: GHC.DerivStrategy GHC.GhcPs -> DerivingStrategy
mkDerivingStrategy GHC.StockStrategy {} = Stock
mkDerivingStrategy GHC.AnyclassStrategy {} = Anyclass
mkDerivingStrategy GHC.NewtypeStrategy {} = Newtype
mkDerivingStrategy (GHC.ViaStrategy (GHC.XViaStrategyPs _ x)) =
  Via $ flattenComments $ mkTypeFromHsSigType <$> mkWithCommentsFromGenLocated x

isViaStrategy :: DerivingStrategy -> Bool
isViaStrategy Via {} = True
isViaStrategy _ = False
