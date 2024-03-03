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

instance Pretty ModuleWarning where
  pretty' ModuleWarning {messages = [msg], ..} = do
    string "{-# "
    case kind of
      Warning    -> string "WARNING"
      Deprecated -> string "DEPRECATED"
    space
    string msg
    string " #-}"
  pretty' ModuleWarning {..} = do
    string "{-# "
    case kind of
      Warning    -> string "WARNING"
      Deprecated -> string "DEPRECATED"
    space
    hList $ fmap string messages
    string " #-}"

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
