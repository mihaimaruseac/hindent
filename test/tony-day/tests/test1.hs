{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings, MultiWayIf #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-type-defaults #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fno-warn-type-defaults -fno-warn-name-shadowing #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- top comment

module Test1 where

import qualified HIndent
import HIndent
import qualified HIndent as H
import HIndent.Styles.TonyDay
import           Language.Haskell.Exts.Annotated
import           Language.Haskell.Exts.Annotated hiding (Style,prettyPrint,Pretty,style,parse)
import Control.Monad
import Control.Monad (unless)
import Control.Monad (when)

import qualified Data.Foldable as F
-- import Control.Lens hiding (each)
-- import Data.List
import Data.Monoid

-- floating between imps and decls

-- before decl1
x1=1
-- before decl2
x2=2
-- trailing

