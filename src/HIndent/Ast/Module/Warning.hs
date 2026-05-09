{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Module.Warning
  ( ModuleWarning
  , mkModuleWarning
  ) where

import qualified GHC.Types.SourceText as GHC
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.Declaration.Warning.Kind
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import qualified HIndent.GhcLibParserWrapper.GHC.Unit.Module.Warnings as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
data ModuleWarning = ModuleWarning
  { messages :: [GHC.StringLiteral]
  , kind :: Kind
  }
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
data ModuleWarning = ModuleWarning
  { messages :: [GHC.StringLiteral]
  , kind :: Kind
  }
#else
data ModuleWarning = ModuleWarning
  { messages :: [GHC.StringLiteral]
  , kind :: Kind
  }
#endif
instance CommentExtraction ModuleWarning where
  nodeComments _ = NodeComments [] [] []

instance Pretty ModuleWarning where
  pretty' ModuleWarning {..} =
    spaced [string "{-#", pretty kind, prettyMsgs, string "#-}"]
    where
      prettyMsgs =
        case messages of
          [x] -> pretty x
          xs -> hList $ fmap pretty xs

mkModuleWarning :: GHC.HsModule' -> Maybe (WithComments ModuleWarning)
mkModuleWarning =
  fmap (fromGenLocated . fmap fromWarningTxt) . GHC.getDeprecMessage

fromWarningTxt :: GHC.WarningTxt' -> ModuleWarning
#if MIN_VERSION_ghc_lib_parser(9, 8, 1)
fromWarningTxt (GHC.WarningTxt _ _ warningMessages) = ModuleWarning {..}
  where
    kind = Warning
    messages = fmap (GHC.hsDocString . GHC.unLoc) warningMessages
#else
fromWarningTxt (GHC.WarningTxt _ warningMessages) = ModuleWarning {..}
  where
    kind = Warning
    messages = fmap (GHC.hsDocString . GHC.unLoc) warningMessages
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
fromWarningTxt (GHC.DeprecatedTxt _ warningMessages) = ModuleWarning {..}
  where
    kind = Deprecated
    messages = fmap (GHC.hsDocString . GHC.unLoc) warningMessages
#elif MIN_VERSION_ghc_lib_parser(9, 4, 1)
fromWarningTxt (GHC.DeprecatedTxt _ warningMessages) = ModuleWarning {..}
  where
    kind = Deprecated
    messages = fmap (GHC.hsDocString . GHC.unLoc) warningMessages
#else
fromWarningTxt (GHC.DeprecatedTxt _ warningMessages) = ModuleWarning {..}
  where
    kind = Deprecated
    messages = fmap GHC.unLoc warningMessages
#endif
