{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Data.GADT.Constructor
  ( GADTConstructor
  , mkGADTConstructor
  ) where

import Data.Maybe
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.Context
import HIndent.Ast.Declaration.Data.GADT.Constructor.Signature
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 6, 0)
import qualified Data.List.NonEmpty as NE
#endif
data GADTConstructor = GADTConstructor
  { names :: [WithComments (GHC.IdP GHC.GhcPs)]
  , forallNeeded :: Bool
  , bindings :: WithComments (GHC.HsOuterSigTyVarBndrs GHC.GhcPs)
  , context :: Maybe (WithComments Context)
  , signature :: ConstructorSignature
  }

instance CommentExtraction GADTConstructor where
  nodeComments GADTConstructor {} = NodeComments [] [] []

instance Pretty GADTConstructor where
  pretty' (GADTConstructor {..}) = do
    hCommaSep $ fmap (`prettyWith` pretty) names
    hor <-|> ver
    where
      hor = string " :: " |=> body
      ver = newline >> indentedBlock (string ":: " |=> body)
      body =
        case (forallNeeded, context) of
          (True, Just ctx) -> withForallCtx ctx
          (True, Nothing) -> withForallOnly
          (False, Just ctx) -> withCtxOnly ctx
          (False, Nothing) -> noForallCtx
      withForallCtx ctx = do
        pretty bindings
        (space >> pretty ctx) <-|> (newline >> pretty ctx)
        newline
        prefixed "=> " $ prettyVertically signature
      withForallOnly = do
        pretty bindings
        (space >> prettyHorizontally signature)
          <-|> (newline >> prettyVertically signature)
      withCtxOnly ctx =
        (pretty ctx >> string " => " >> prettyHorizontally signature)
          <-|> (pretty ctx >> prefixed "=> " (prettyVertically signature))
      noForallCtx = prettyHorizontally signature <-|> prettyVertically signature

mkGADTConstructor :: GHC.ConDecl GHC.GhcPs -> Maybe GADTConstructor
mkGADTConstructor decl@GHC.ConDeclGADT {..} = Just $ GADTConstructor {..}
  where
    names = fromMaybe (error "Couldn't get names.") $ getNames decl
    bindings = fromGenLocated con_bndrs
    forallNeeded =
      case GHC.unLoc con_bndrs of
        GHC.HsOuterImplicit {} -> False
        GHC.HsOuterExplicit {} -> True
    signature =
      fromMaybe (error "Couldn't get signature.") $ mkConstructorSignature decl
    context = fmap (fmap mkContext . fromGenLocated) con_mb_cxt
mkGADTConstructor _ = Nothing

getNames :: GHC.ConDecl GHC.GhcPs -> Maybe [WithComments (GHC.IdP GHC.GhcPs)]
#if MIN_VERSION_ghc_lib_parser(9, 6, 0)
getNames GHC.ConDeclGADT {..} = Just $ NE.toList $ fmap fromGenLocated con_names
#else
getNames GHC.ConDeclGADT {..} = Just $ fmap fromGenLocated con_names
#endif
getNames _ = Nothing
