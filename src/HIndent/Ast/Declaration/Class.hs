{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Class
  ( ClassDeclaration(..)
  , NameAndTypeVariables(..)
  , mkClassDeclaration
  ) where

import qualified GHC.Types.Fixity                   as GHC
import           HIndent.Ast.Context
import           HIndent.Ast.NodeComments
import           HIndent.Ast.Type.Variable
import           HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import           HIndent.Pretty.NodeComments

data NameAndTypeVariables
  = Prefix
      { name          :: GHC.LIdP GHC.GhcPs
      , typeVariables :: [WithComments TypeVariable]
      }
  | Infix
      { left    :: WithComments TypeVariable
      , name    :: GHC.LIdP GHC.GhcPs
      , right   :: WithComments TypeVariable
      , remains :: [WithComments TypeVariable]
      }

data ClassDeclaration = ClassDeclaration
  { context              :: Maybe (WithComments Context)
  , nameAndTypeVariables :: NameAndTypeVariables
  , decl                 :: GHC.TyClDecl GHC.GhcPs
  }

instance CommentExtraction NameAndTypeVariables where
  nodeComments Prefix {} = NodeComments [] [] []
  nodeComments Infix {}  = NodeComments [] [] []

instance CommentExtraction ClassDeclaration where
  nodeComments ClassDeclaration {} = NodeComments [] [] []

mkNameAndTypeVariables :: GHC.TyClDecl GHC.GhcPs -> Maybe NameAndTypeVariables
mkNameAndTypeVariables GHC.ClassDecl {tcdFixity = GHC.Prefix, ..} =
  Just Prefix {..}
  where
    name = tcdLName
    typeVariables =
      fmap mkTypeVariable . fromGenLocated <$> GHC.hsq_explicit tcdTyVars
mkNameAndTypeVariables GHC.ClassDecl { tcdFixity = GHC.Infix
                                     , tcdTyVars = GHC.HsQTvs {hsq_explicit = h:t:xs}
                                     , ..
                                     } = Just Infix {..}
  where
    left = mkTypeVariable <$> fromGenLocated h
    name = tcdLName
    right = mkTypeVariable <$> fromGenLocated t
    remains = fmap (fmap mkTypeVariable . fromGenLocated) xs
mkNameAndTypeVariables _ = Nothing

mkClassDeclaration :: GHC.TyClDecl GHC.GhcPs -> Maybe ClassDeclaration
mkClassDeclaration x@GHC.ClassDecl {..}
  | Just nameAndTypeVariables <- mkNameAndTypeVariables x =
    Just ClassDeclaration {..}
  where
    context = fmap (fmap mkContext . fromGenLocated) tcdCtxt
    decl = x
mkClassDeclaration _ = Nothing
