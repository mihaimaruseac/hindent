{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Data.Header
  ( Header
  , mkHeader
  ) where

import HIndent.Applicative
import HIndent.Ast.Context
import HIndent.Ast.Declaration.Data.NewOrData
import HIndent.Ast.Name.Prefix
import HIndent.Ast.Type.Variable
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators

data Header = Header
  { newOrData :: NewOrData
  , name :: WithComments PrefixName
  , context :: Maybe (WithComments Context)
  , typeVariables :: [WithComments TypeVariable]
  }

instance Pretty Header where
  pretty Header {..} = do
    (pretty newOrData >> space) |=> do
      whenJust context $ \c -> pretty c >> string " =>" >> newline
      pretty name
    spacePrefixed $ fmap pretty typeVariables

mkHeader :: GHC.TyClDecl GHC.GhcPs -> Maybe Header
mkHeader GHC.DataDecl {tcdDataDefn = defn@GHC.HsDataDefn {..}, ..} =
  Just Header {..}
  where
    newOrData = mkNewOrData defn
    context = fmap (fmap mkContext . mkWithCommentsFromGenLocated) dd_ctxt
    name = mkWithCommentsFromGenLocated $ fmap mkPrefixName tcdLName
    typeVariables =
      fmap mkTypeVariable . mkWithCommentsFromGenLocated
        <$> GHC.hsq_explicit tcdTyVars
mkHeader _ = Nothing
