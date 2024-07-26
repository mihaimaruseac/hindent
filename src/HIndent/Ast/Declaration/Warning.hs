{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Warning
  ( WarningDeclaration
  , mkWarningDeclaration
  ) where

import qualified GHC.Types.SourceText as GHC
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.Declaration.Warning.Kind
import HIndent.Ast.Name.Prefix
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import qualified HIndent.GhcLibParserWrapper.GHC.Unit.Module.Warnings as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data WarningDeclaration = WarningDeclaration
  { names :: [WithComments PrefixName]
  , kind :: Kind
  , reasons :: [GHC.Located GHC.StringLiteral]
  }

instance CommentExtraction WarningDeclaration where
  nodeComments _ = NodeComments [] [] []

instance Pretty WarningDeclaration where
  pretty' WarningDeclaration {..} = do
    lined
      [ string "{-# " >> pretty kind
      , spaced [hCommaSep $ fmap pretty names, hCommaSep $ fmap pretty reasons]
      , string " #-}"
      ]

mkWarningDeclaration :: GHC.WarnDecl GHC.GhcPs -> WarningDeclaration
#if MIN_VERSION_ghc_lib_parser(9, 8, 1)
mkWarningDeclaration (GHC.Warning _ ns (GHC.DeprecatedTxt _ rs)) =
  WarningDeclaration {kind = Deprecated, ..}
  where
    names = fmap (fromGenLocated . fmap mkPrefixName) ns
    reasons = fmap (fmap GHC.hsDocString) rs
mkWarningDeclaration (GHC.Warning _ ns (GHC.WarningTxt _ _ rs)) =
  WarningDeclaration {kind = Warning, ..}
  where
    names = fmap (fromGenLocated . fmap mkPrefixName) ns
    reasons = fmap (fmap GHC.hsDocString) rs
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
mkWarningDeclaration (GHC.Warning _ ns (GHC.DeprecatedTxt _ rs)) =
  WarningDeclaration {kind = Deprecated, ..}
  where
    names = fmap (fromGenLocated . fmap mkPrefixName) ns
    reasons = fmap (fmap GHC.hsDocString) rs
mkWarningDeclaration (GHC.Warning _ ns (GHC.WarningTxt _ rs)) =
  WarningDeclaration {kind = Warning, ..}
  where
    names = fmap (fromGenLocated . fmap mkPrefixName) ns
    reasons = fmap (fmap GHC.hsDocString) rs
#else
mkWarningDeclaration (GHC.Warning _ ns (GHC.DeprecatedTxt _ reasons)) =
  WarningDeclaration {kind = Deprecated, ..}
  where
    names = fmap (fromGenLocated . fmap mkPrefixName) ns
mkWarningDeclaration (GHC.Warning _ ns (GHC.WarningTxt _ reasons)) =
  WarningDeclaration {kind = Warning, ..}
  where
    names = fmap (fromGenLocated . fmap mkPrefixName) ns
#endif
