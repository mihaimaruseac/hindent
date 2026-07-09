{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Module.Warning
  ( ModuleWarning
  , mkModuleWarning
  ) where

import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.Declaration.Warning.Kind
import HIndent.Ast.TextValue
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import qualified HIndent.GhcLibParserWrapper.GHC.Unit.Module.Warnings as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators

data ModuleWarning = ModuleWarning
  { messages :: [TextValue]
  , kind :: Kind
  }

instance Pretty ModuleWarning where
  pretty ModuleWarning {..} =
    spaced [string "{-#", pretty kind, prettyMsgs, string "#-}"]
    where
      prettyMsgs =
        case messages of
          [x] -> pretty x
          xs -> hList $ fmap pretty xs

mkModuleWarning :: GHC.HsModule' -> Maybe (WithComments ModuleWarning)
mkModuleWarning =
  fmap (mkWithCommentsFromGenLocated . fmap fromWarningTxt)
    . GHC.getDeprecMessage

fromWarningTxt :: GHC.WarningTxt' -> ModuleWarning
#if MIN_VERSION_ghc_lib_parser(9, 8, 1)
fromWarningTxt (GHC.WarningTxt _ _ warningMessages) =
  ModuleWarning
    { kind = Warning
    , messages =
        fmap
          (mkTextValueFromStringLiteral . GHC.hsDocString . GHC.unLoc)
          warningMessages
    }
#else
fromWarningTxt (GHC.WarningTxt _ warningMessages) =
  ModuleWarning
    { kind = Warning
    , messages =
        fmap
          (mkTextValueFromStringLiteral . GHC.hsDocString . GHC.unLoc)
          warningMessages
    }
#endif
#if MIN_VERSION_ghc_lib_parser(9, 10, 1)
fromWarningTxt (GHC.DeprecatedTxt _ warningMessages) =
  ModuleWarning
    { kind = Deprecated
    , messages =
        fmap
          (mkTextValueFromStringLiteral . GHC.hsDocString . GHC.unLoc)
          warningMessages
    }
#else
fromWarningTxt (GHC.DeprecatedTxt _ warningMessages) =
  ModuleWarning
    { kind = Deprecated
    , messages =
        fmap
          (mkTextValueFromStringLiteral . GHC.hsDocString . GHC.unLoc)
          warningMessages
    }
#endif
