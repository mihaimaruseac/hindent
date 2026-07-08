{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Name.Prefix
  ( PrefixName
  , mkPrefixName
  , fromString
  , PrefixAsInfix
  , mkPrefixAsInfix
  , prefixAsInfixFixity
  ) where

import Data.Maybe
import qualified Data.Text as Text
import qualified GHC.Types.Fixity as Fixity
import qualified GHC.Types.Name as GHC
import qualified GHC.Types.Name.Reader as GHC
import HIndent.Ast.Module.Name
import HIndent.Ast.TextValue (TextValue, mkTextValueFromString, toText)
import HIndent.Fixity (fixities)
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators

data PrefixName = PrefixName
  { name :: TextValue
  , moduleName :: Maybe ModuleName
  , parentheses :: Bool
  }

instance Pretty PrefixName where
  pretty PrefixName {..} =
    wrap $ hDotSep $ catMaybes [pretty <$> moduleName, Just $ pretty name]
    where
      wrap =
        if parentheses
          then parens
          else id

mkPrefixName :: GHC.RdrName -> PrefixName
mkPrefixName (GHC.Unqual name) =
  PrefixName
    { name = mkTextValueFromString $ showOutputable name
    , moduleName = Nothing
    , parentheses = parensNeeded name
    }
mkPrefixName (GHC.Qual modName name) =
  PrefixName
    { name = mkTextValueFromString $ showOutputable name
    , moduleName = Just $ mkModuleName modName
    , parentheses = parensNeeded name
    }
mkPrefixName (GHC.Orig {}) =
  error "This AST node should not appear in the parser output."
mkPrefixName (GHC.Exact name) =
  PrefixName
    { name = mkTextValueFromString $ showOutputable name
    , moduleName = Nothing
    , parentheses = parensNeeded $ GHC.occName name
    }

fromString :: String -> PrefixName
fromString name =
  PrefixName
    { name = mkTextValueFromString name
    , moduleName = Nothing
    , parentheses = False
    }

newtype PrefixAsInfix =
  PrefixAsInfix PrefixName

instance Pretty PrefixAsInfix where
  pretty (PrefixAsInfix PrefixName {..}) =
    wrap $ hDotSep $ catMaybes [pretty <$> moduleName, Just $ pretty name]
    where
      wrap =
        if parentheses
          then id
          else backticks

mkPrefixAsInfix :: PrefixName -> PrefixAsInfix
mkPrefixAsInfix = PrefixAsInfix

prefixAsInfixFixity :: PrefixAsInfix -> Fixity.Fixity
prefixAsInfixFixity (PrefixAsInfix PrefixName {..}) =
  fromMaybe Fixity.defaultFixity $ lookup (Text.unpack $ toText name) fixities

parensNeeded :: GHC.OccName -> Bool
parensNeeded = GHC.isSymOcc
