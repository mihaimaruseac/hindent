{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-|
  Merge 2 haskell modules.
-}

module HIndent.Merge where

import HIndent.Styles.TonyDay

import Data.Monoid
import Language.Haskell.Exts.Annotated

-- | sort of a mappend anyway
maybeMappend :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
maybeMappend ma x x' =
  case x of
    Nothing -> case x' of
      Nothing -> Nothing
      Just v' -> Just v'
    Just v -> case x' of
      Nothing -> Just v
      Just v' -> Just (ma v v')

-- | favours the first module header, but merges the export list (but no duplicate check)
mergeModuleHead :: ModuleHead a -> ModuleHead a -> ModuleHead a
mergeModuleHead (ModuleHead a name warn exports)
                (ModuleHead _ _ warn' exports') =
  ModuleHead a name (maybeMappend const warn warn')
  (maybeMappend (\(ExportSpecList a es) (ExportSpecList _ es') -> ExportSpecList a (es<>es')) exports exports')

-- | full merge of two modules
mergeModule :: (Show a) => Module a -> Module a -> Module a
mergeModule (Module a head pragmas imps decls)
            (Module _ head' pragmas' imps' decls') =
  clean (Module a (maybeMappend mergeModuleHead head head')  (pragmas<>pragmas')
         (imps <> imps') (decls <> decls'))

