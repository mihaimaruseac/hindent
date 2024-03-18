{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Data.Haskell98.Constructor
  ( Haskell98Constructor
  , mkHaskell98Constructor
  , hasSingleRecordConstructor
  ) where

import qualified GHC.Core.Type as GHC
import HIndent.Applicative
import HIndent.Ast.Context
import HIndent.Ast.Declaration.Data.Haskell98.Constructor.Body
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data Haskell98Constructor = Haskell98Constructor
  { existentialVariables :: [GHC.LHsTyVarBndr GHC.Specificity GHC.GhcPs]
  , context :: Maybe (WithComments Context)
  , body :: Haskell98ConstructorBody
  }

instance CommentExtraction Haskell98Constructor where
  nodeComments Haskell98Constructor {} = NodeComments [] [] []

instance Pretty Haskell98Constructor where
  pretty' Haskell98Constructor {existentialVariables = [], ..} = do
    whenJust context $ \c -> pretty c >> string " => "
    pretty body
  pretty' Haskell98Constructor {..} = do
    string "forall "
    spaced (fmap pretty existentialVariables)
    string ". " |=> do
      whenJust context $ \c -> pretty c >> string " =>" >> newline
      pretty body

mkHaskell98Constructor :: GHC.ConDecl GHC.GhcPs -> Maybe Haskell98Constructor
mkHaskell98Constructor GHC.ConDeclH98 {..}
  | Just body <- mkHaskell98ConstructorBody GHC.ConDeclH98 {..} =
    Just Haskell98Constructor {..}
  where
    existentialVariables =
      if con_forall
        then con_ex_tvs
        else []
    context = fmap (fmap mkContext . fromGenLocated) con_mb_cxt
mkHaskell98Constructor _ = Nothing

hasSingleRecordConstructor :: Haskell98Constructor -> Bool
hasSingleRecordConstructor = isRecord . body
