{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Data.Header
  ( Header
  , mkHeader
  ) where

import           HIndent.Ast.Declaration.Data.NewOrData
import           HIndent.Ast.NodeComments
import           HIndent.Ast.Type.Variable
import           HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs     as GHC
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments
import           HIndent.Pretty.Types

data Header = Header
  { newOrData     :: NewOrData
  , name          :: WithComments (GHC.IdP GHC.GhcPs)
  , context       :: Context
  , typeVariables :: [WithComments TypeVariable]
  }

instance CommentExtraction Header where
  nodeComments Header {} = NodeComments [] [] []

instance Pretty Header where
  pretty' Header {..} = do
    (pretty newOrData >> space) |=> do
      case context of
        Context (Just _) -> pretty context >> string " =>" >> newline
        Context Nothing  -> pure ()
      pretty name
    spacePrefixed $ fmap pretty typeVariables

mkHeader :: GHC.TyClDecl GHC.GhcPs -> Maybe Header
mkHeader GHC.DataDecl {tcdDataDefn = GHC.HsDataDefn {..}, ..} = Just Header {..}
  where
    newOrData = mkNewOrData dd_ND
    context = Context dd_ctxt
    name = fromGenLocated tcdLName
    typeVariables =
      fmap mkTypeVariable . fromGenLocated <$> GHC.hsq_explicit tcdTyVars
mkHeader _ = Nothing
