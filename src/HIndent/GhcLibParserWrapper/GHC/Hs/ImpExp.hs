{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.GhcLibParserWrapper.GHC.Hs.ImpExp
  ( module GHC.Hs.ImpExp
  , getPackageName
  ) where

import qualified GHC.Hs as GHC
import GHC.Hs.ImpExp
import qualified GHC.Types.SourceText as GHC
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
import qualified GHC.Types.PkgQual as GHC
#endif
getPackageName :: GHC.ImportDecl GHC.GhcPs -> Maybe GHC.StringLiteral
#if MIN_VERSION_ghc_lib_parser(9, 4, 1)
getPackageName GHC.ImportDecl {GHC.ideclPkgQual = GHC.RawPkgQual name} =
  Just name
getPackageName GHC.ImportDecl {GHC.ideclPkgQual = GHC.NoRawPkgQual} = Nothing
#else
getPackageName GHC.ImportDecl {..} = ideclPkgQual
#endif
