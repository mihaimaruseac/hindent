{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Module.Warning
  ( ModuleWarning
  , mkModuleWarning
  ) where

import           HIndent.Ast.NodeComments
import           HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs                   as GHC
import qualified HIndent.GhcLibParserWrapper.GHC.Unit.Module.Warnings as GHC
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments

data ModuleWarning = ModuleWarning
  { messages :: [String]
  , kind     :: Kind
  }

data Kind
  = Warning
  | Deprecated

instance CommentExtraction ModuleWarning where
  nodeComments _ = NodeComments [] [] []

instance CommentExtraction Kind where
  nodeComments _ = NodeComments [] [] []

instance Pretty Kind where
  pretty' Warning    = string "WARNING"
  pretty' Deprecated = string "DEPRECATED"

instance Pretty ModuleWarning where
  pretty' ModuleWarning {..} = do
    string "{-# "
    pretty kind
    space
    prettyMsgs
    string " #-}"
    where
      prettyMsgs =
        case messages of
          [x] -> string x
          xs  -> hList $ fmap string xs

mkModuleWarning :: GHC.HsModule' -> Maybe (WithComments ModuleWarning)
mkModuleWarning =
  fmap (fromGenLocated . fmap fromWarningTxt) . GHC.getDeprecMessage
  where
    fromWarningTxt (GHC.WarningTxt _ s) = ModuleWarning {..}
      where
        messages = fmap showOutputable s
        kind = Warning
    fromWarningTxt (GHC.DeprecatedTxt _ s) = ModuleWarning {..}
      where
        messages = fmap showOutputable s
        kind = Deprecated
