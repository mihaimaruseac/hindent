{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Name.Infix
  ( InfixName
  , mkInfixName
  , getInfixName
  , unlessSpecialOp
  ) where

import Control.Monad
import Data.Maybe
import Data.Text (Text)
import qualified GHC.Types.Name as GHC
import qualified GHC.Types.Name.Reader as GHC
import HIndent.Ast.Module.Name
import HIndent.Ast.TextValue
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators hiding (unlessSpecialOp)
import HIndent.Printer

data InfixName = InfixName
  { name :: TextValue
  , moduleName :: Maybe ModuleName
  , backtick :: Bool
  }

instance Pretty InfixName where
  pretty InfixName {..} =
    wrap $ hDotSep $ catMaybes [pretty <$> moduleName, Just $ pretty name]
    where
      wrap =
        if backtick
          then backticks
          else id

mkInfixName :: GHC.RdrName -> InfixName
mkInfixName (GHC.Unqual name) =
  InfixName
    { name = mkTextValueFromString $ showOutputable name
    , moduleName = Nothing
    , backtick = backticksNeeded name
    }
mkInfixName (GHC.Qual modName name) =
  InfixName
    { name = mkTextValueFromString $ showOutputable name
    , moduleName = Just $ mkModuleName modName
    , backtick = backticksNeeded name
    }
mkInfixName (GHC.Orig {}) =
  error "This AST node should not appear in the parser output."
mkInfixName (GHC.Exact name) =
  InfixName
    { name = mkTextValueFromString $ showOutputable $ GHC.occName name
    , moduleName = Nothing
    , backtick = backticksNeeded $ GHC.occName name
    }

getInfixName :: InfixName -> Text
getInfixName = toText . name

unlessSpecialOp :: InfixName -> Printer () -> Printer ()
unlessSpecialOp InfixName {..} =
  unless $ toText name `elem` ["()", "[]", "->", ":"]

backticksNeeded :: GHC.OccName -> Bool
backticksNeeded = not . GHC.isSymOcc
