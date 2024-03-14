{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Class
  ( ClassDeclaration(..)
  , mkClassDeclaration
  ) where

import qualified GHC.Data.Bag as GHC
import HIndent.Ast.Context
import HIndent.Ast.Declaration.Class.FunctionalDependency
import HIndent.Ast.Declaration.Class.NameAndTypeVariables
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
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
