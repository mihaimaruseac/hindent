{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Class
  ( ClassDeclaration
  , mkClassDeclaration
  ) where

import Control.Monad
import Data.Maybe
import HIndent.Applicative
import HIndent.Ast.Context
import HIndent.Ast.Declaration.Class.AssociatedThings
  ( AssociatedThings
  , mkAssociatedThings
  , nullAssociatedThings
  )
import HIndent.Ast.Declaration.Class.FunctionalDependency
import HIndent.Ast.Declaration.Class.NameAndTypeVariables
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
#if !MIN_VERSION_ghc_lib_parser(9, 12, 1)
import qualified GHC.Data.Bag as GHC
#endif
data ClassDeclaration = ClassDeclaration
  { context :: Maybe (WithComments Context)
  , nameAndTypeVariables :: NameAndTypeVariables
  , functionalDependencies :: [WithComments FunctionalDependency]
  , associatedThings :: AssociatedThings
  }

instance CommentExtraction ClassDeclaration where
  nodeComments ClassDeclaration {} = NodeComments [] [] []

instance Pretty ClassDeclaration where
  pretty' ClassDeclaration {..} = do
    if isJust context
      then verHead
      else horHead <-|> verHead
    indentedBlock $ pretty associatedThings
    where
      horHead = do
        string "class "
        pretty nameAndTypeVariables
        unless (null functionalDependencies)
          $ string " | " >> hCommaSep (fmap pretty functionalDependencies)
        unless (nullAssociatedThings associatedThings) $ string " where"
      verHead = do
        string "class " |=> do
          whenJust context $ \ctx -> pretty ctx >> string " =>" >> newline
          pretty nameAndTypeVariables
        unless (null functionalDependencies) $ do
          newline
          indentedBlock
            $ string "| " |=> vCommaSep (fmap pretty functionalDependencies)
        unless (nullAssociatedThings associatedThings)
          $ newline >> indentedBlock (string "where")

mkClassDeclaration :: GHC.TyClDecl GHC.GhcPs -> Maybe ClassDeclaration
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkClassDeclaration x@GHC.ClassDecl {..}
  | Just nameAndTypeVariables <- mkNameAndTypeVariables x =
    Just ClassDeclaration {..}
  where
    context = fmap (fmap mkContext . fromGenLocated) tcdCtxt
    functionalDependencies =
      fmap (fmap mkFunctionalDependency . fromGenLocated) tcdFDs
    associatedThings = mkAssociatedThings tcdSigs tcdMeths tcdATs tcdATDefs
#else
mkClassDeclaration x@GHC.ClassDecl {..}
  | Just nameAndTypeVariables <- mkNameAndTypeVariables x =
    Just ClassDeclaration {..}
  where
    context = fmap (fmap mkContext . fromGenLocated) tcdCtxt
    functionalDependencies =
      fmap (fmap mkFunctionalDependency . fromGenLocated) tcdFDs
    associatedThings =
      mkAssociatedThings tcdSigs (GHC.bagToList tcdMeths) tcdATs tcdATDefs
#endif
mkClassDeclaration _ = Nothing
