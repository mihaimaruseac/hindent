{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Name.Infix
  ( InfixName
  , mkInfixName
  , getInfixName
  , unlessSpecialOp
  ) where

import Control.Monad
import Data.Maybe
import qualified GHC.Types.Name as GHC
import qualified GHC.Types.Name.Reader as GHC
import HIndent.Ast.Module.Name
import HIndent.Ast.NodeComments
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators hiding (unlessSpecialOp)
import HIndent.Pretty.NodeComments
import HIndent.Printer

data InfixName = InfixName
  { name :: String
  , moduleName :: Maybe ModuleName
  , backtick :: Bool
  }

instance CommentExtraction InfixName where
  nodeComments InfixName {} = NodeComments [] [] []

instance Pretty InfixName where
  pretty' InfixName {..} =
    wrap $ hDotSep $ catMaybes [pretty <$> moduleName, Just $ string name]
    where
      wrap =
        if backtick
          then backticks
          else id

mkInfixName :: GHC.RdrName -> InfixName
mkInfixName (GHC.Unqual name) =
  InfixName (showOutputable name) Nothing (backticksNeeded name)
mkInfixName (GHC.Qual modName name) =
  InfixName
    (showOutputable name)
    (Just $ mkModuleName modName)
    (backticksNeeded name)
mkInfixName (GHC.Orig {}) =
  error "This AST node should not appear in the parser output."
mkInfixName (GHC.Exact name) =
  InfixName
    (showOutputable $ GHC.occName name)
    Nothing
    (backticksNeeded $ GHC.occName name)

getInfixName :: InfixName -> String
getInfixName = name

unlessSpecialOp :: InfixName -> Printer () -> Printer ()
unlessSpecialOp InfixName {..} = unless $ name `elem` ["()", "[]", "->", ":"]

backticksNeeded :: GHC.OccName -> Bool
backticksNeeded = not . GHC.isSymOcc
