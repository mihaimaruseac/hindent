{-# LANGUAGE CPP #-}

module HIndent.Ast.TextValue
  ( TextValue
  , mkTextValueFromString
  , mkTextValueFromText
  , mkTextValueFromStringLiteral
  , toText
  ) where

import qualified Data.Text as Text
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
import qualified GHC.Data.FastString as GHC
#endif
import qualified GHC.Types.SourceText as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators

newtype TextValue =
  TextValue Text.Text
  deriving (Eq, Ord, Show)

instance Pretty TextValue where
  pretty (TextValue value) = string value

mkTextValueFromString :: String -> TextValue
mkTextValueFromString = TextValue . Text.pack

mkTextValueFromText :: Text.Text -> TextValue
mkTextValueFromText = TextValue

mkTextValueFromStringLiteral :: GHC.StringLiteral -> TextValue
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
mkTextValueFromStringLiteral GHC.StringLiteral {GHC.sl_st = GHC.SourceText value} =
  mkTextValueFromString $ GHC.unpackFS value
mkTextValueFromStringLiteral GHC.StringLiteral {GHC.sl_fs = value} =
  mkTextValueFromString $ GHC.unpackFS value
#else
mkTextValueFromStringLiteral literal =
  mkTextValueFromString $ showOutputable literal
#endif
toText :: TextValue -> Text.Text
toText (TextValue value) = value
