{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Class
  ( ClassDeclaration
  , mkClassDeclaration
  ) where

import Control.Monad
import Data.Maybe
import qualified GHC.Data.Bag as GHC
import HIndent.Applicative
import HIndent.Ast.Context
import HIndent.Ast.Declaration.Class.FunctionalDependency
import HIndent.Ast.Declaration.Class.NameAndTypeVariables
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Pretty.SigBindFamily

data ClassDeclaration = ClassDeclaration
  { context :: Maybe (WithComments Context)
  , nameAndTypeVariables :: NameAndTypeVariables
  , functionalDependencies :: [WithComments FunctionalDependency]
  , associatedThings :: [LSigBindFamily]
  }

instance CommentExtraction ClassDeclaration where
  nodeComments ClassDeclaration {} = NodeComments [] [] []

instance Pretty ClassDeclaration where
  pretty' ClassDeclaration {..} = do
    if isJust context
      then verHead
      else horHead <-|> verHead
    indentedBlock $ newlinePrefixed $ fmap pretty associatedThings
    where
      horHead = do
        string "class "
        pretty nameAndTypeVariables
        unless (null functionalDependencies)
          $ string " | " >> hCommaSep (fmap pretty functionalDependencies)
        unless (null associatedThings) $ string " where"
      verHead = do
        string "class " |=> do
          whenJust context $ \ctx -> pretty ctx >> string " =>" >> newline
          pretty nameAndTypeVariables
        unless (null functionalDependencies) $ do
          newline
          indentedBlock
            $ string "| " |=> vCommaSep (fmap pretty functionalDependencies)
        unless (null associatedThings)
          $ newline >> indentedBlock (string "where")

mkClassDeclaration :: GHC.TyClDecl GHC.GhcPs -> Maybe ClassDeclaration
mkClassDeclaration x@GHC.ClassDecl {..}
  | Just nameAndTypeVariables <- mkNameAndTypeVariables x =
    Just ClassDeclaration {..}
  where
    context = fmap (fmap mkContext . fromGenLocated) tcdCtxt
    functionalDependencies =
      fmap (fmap mkFunctionalDependency . fromGenLocated) tcdFDs
    associatedThings =
      mkSortedLSigBindFamilyList tcdSigs (GHC.bagToList tcdMeths) tcdATs [] []
mkClassDeclaration _ = Nothing
