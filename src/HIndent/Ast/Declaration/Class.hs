{-# LANGUAGE OverloadedStrings #-}
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
import HIndent.Ast.Declaration.Class.Body
import HIndent.Ast.Declaration.Class.FunctionalDependency
import HIndent.Ast.Declaration.Class.NameAndTypeVariables
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators

#if !MIN_VERSION_ghc_lib_parser(9, 12, 1)
import qualified GHC.Data.Bag as GHC
#endif

data ClassDeclaration = ClassDeclaration
  { context :: Maybe (WithComments Context)
  , nameAndTypeVariables :: NameAndTypeVariables
  , functionalDependencies :: [WithComments FunctionalDependency]
  , body :: ClassBody
  }

instance Pretty ClassDeclaration where
  pretty ClassDeclaration {..} = do
    if isJust context
      then verHead
      else horHead <-|> verHead
    indentedBlock $ pretty body
    where
      horHead = do
        string "class "
        pretty nameAndTypeVariables
        unless (null functionalDependencies)
          $ string " | " >> hCommaSep (fmap pretty functionalDependencies)
        when (hasClassBody body) $ string " where"
      verHead = do
        string "class " |=> do
          whenJust context $ \ctx -> pretty ctx >> string " =>" >> newline
          pretty nameAndTypeVariables
        unless (null functionalDependencies) $ do
          newline
          indentedBlock
            $ string "| " |=> vCommaSep (fmap pretty functionalDependencies)
        when (hasClassBody body) $ newline >> indentedBlock (string "where")

mkClassDeclaration :: GHC.TyClDecl GHC.GhcPs -> Maybe ClassDeclaration
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkClassDeclaration x@GHC.ClassDecl {..}
  | Just nameAndTypeVariables <- mkNameAndTypeVariables x =
    Just ClassDeclaration {..}
  where
    context = fmap (fmap mkContext . mkWithCommentsFromGenLocated) tcdCtxt
    functionalDependencies =
      fmap (fmap mkFunctionalDependency . mkWithCommentsFromGenLocated) tcdFDs
    body = mkClassBody tcdSigs tcdMeths tcdATs tcdATDefs
#else
mkClassDeclaration x@GHC.ClassDecl {..}
  | Just nameAndTypeVariables <- mkNameAndTypeVariables x =
    Just ClassDeclaration {..}
  where
    context = fmap (fmap mkContext . mkWithCommentsFromGenLocated) tcdCtxt
    functionalDependencies =
      fmap (fmap mkFunctionalDependency . mkWithCommentsFromGenLocated) tcdFDs
    body = mkClassBody tcdSigs (GHC.bagToList tcdMeths) tcdATs tcdATDefs
#endif

mkClassDeclaration _ = Nothing
