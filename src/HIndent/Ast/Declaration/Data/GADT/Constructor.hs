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
import HIndent.Ast.Name.Prefix
import HIndent.Ast.NodeComments
import HIndent.Ast.Type.Variable
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 6, 0)
import qualified Data.List.NonEmpty as NE
#endif
data GADTConstructor = GADTConstructor
  { names :: [WithComments PrefixName]
  , bindings :: Maybe (WithComments [WithComments TypeVariable])
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
        case (bindings, context) of
          (Just bs, Just ctx) -> withForallCtx bs ctx
          (Just bs, Nothing) -> withForallOnly bs
          (Nothing, Just ctx) -> withCtxOnly ctx
          (Nothing, Nothing) -> noForallCtx
      withForallCtx bs ctx = do
        string "forall"
        prettyWith bs (spacePrefixed . fmap pretty)
        dot
        (space >> pretty ctx) <-|> (newline >> pretty ctx)
        newline
        prefixed "=> " $ prettyVertically signature
      withForallOnly bs = do
        string "forall"
        prettyWith bs (spacePrefixed . fmap pretty)
        dot
        (space >> prettyHorizontally signature)
          <-|> (newline >> prettyVertically signature)
      withCtxOnly ctx =
        (pretty ctx >> string " => " >> prettyHorizontally signature)
          <-|> (pretty ctx >> prefixed "=> " (prettyVertically signature))
      noForallCtx = prettyHorizontally signature <-|> prettyVertically signature

mkGADTConstructor :: GHC.ConDecl GHC.GhcPs -> Maybe GADTConstructor
#if MIN_VERSION_ghc_lib_parser(9, 14, 0)
mkGADTConstructor decl@GHC.ConDeclGADT {..} = Just $ GADTConstructor {..}
  where
    names = fromMaybe (error "Couldn't get names.") $ getNames decl
    bindings =
      case con_outer_bndrs of
        GHC.L _ GHC.HsOuterImplicit {} -> Nothing
        GHC.L l GHC.HsOuterExplicit {..} ->
          Just
            $ fromGenLocated
            $ fmap
                (fmap (fmap mkTypeVariable . fromGenLocated))
                (GHC.L l hso_bndrs)
    signature =
      fromMaybe (error "Couldn't get signature.") $ mkConstructorSignature decl
    context = fmap (fmap mkContext . fromGenLocated) con_mb_cxt
#else
mkGADTConstructor decl@GHC.ConDeclGADT {..} = Just $ GADTConstructor {..}
  where
    names = fromMaybe (error "Couldn't get names.") $ getNames decl
    bindings =
      case con_bndrs of
        GHC.L _ GHC.HsOuterImplicit {} -> Nothing
        GHC.L l GHC.HsOuterExplicit {..} ->
          Just
            $ fromGenLocated
            $ fmap
                (fmap (fmap mkTypeVariable . fromGenLocated))
                (GHC.L l hso_bndrs)
    signature =
      fromMaybe (error "Couldn't get signature.") $ mkConstructorSignature decl
    context = fmap (fmap mkContext . fromGenLocated) con_mb_cxt
#endif
mkGADTConstructor _ = Nothing

getNames :: GHC.ConDecl GHC.GhcPs -> Maybe [WithComments PrefixName]
#if MIN_VERSION_ghc_lib_parser(9, 6, 0)
getNames GHC.ConDeclGADT {..} =
  Just $ NE.toList $ fmap (fromGenLocated . fmap mkPrefixName) con_names
#else
getNames GHC.ConDeclGADT {..} =
  Just $ fmap (fromGenLocated . fmap mkPrefixName) con_names
#endif
getNames _ = Nothing
