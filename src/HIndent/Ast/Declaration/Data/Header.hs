{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Data.Header
  ( Header
  , mkHeader
  ) where

import HIndent.Applicative
import HIndent.Ast.Context
import HIndent.Ast.Declaration.Data.NewOrData
import HIndent.Ast.NodeComments
import HIndent.Ast.Type.Variable
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data Header = Header
  { newOrData :: NewOrData
  , name :: WithComments (GHC.IdP GHC.GhcPs)
  , context :: Maybe (WithComments Context)
  , typeVariables :: [WithComments TypeVariable]
  }

instance CommentExtraction Header where
  nodeComments Header {} = NodeComments [] [] []

instance Pretty Header where
  pretty' Header {..} = do
    (pretty newOrData >> space) |=> do
      whenJust context $ \c -> pretty c >> string " =>" >> newline
      pretty name
    spacePrefixed $ fmap pretty typeVariables

mkHeader :: GHC.TyClDecl GHC.GhcPs -> Maybe Header
mkHeader GHC.DataDecl {tcdDataDefn = defn@GHC.HsDataDefn {..}, ..} =
  Just Header {..}
  where
    newOrData = mkNewOrData defn
    context = fmap (fmap mkContext . fromGenLocated) dd_ctxt
    name = fromGenLocated tcdLName
    typeVariables =
      fmap mkTypeVariable . fromGenLocated <$> GHC.hsq_explicit tcdTyVars
mkHeader _ = Nothing
