{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Data.GADT.Constructor
  ( GADTConstructor
  , mkGADTConstructor
  ) where

import           Data.Maybe
import qualified GHC.Types.SrcLoc                                        as GHC
import           HIndent.Ast.Declaration.Data.GADT.Constructor.Signature
import           HIndent.Ast.NodeComments
import           HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs                      as GHC
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments
import           HIndent.Pretty.Types
#if MIN_VERSION_ghc_lib_parser(9, 4, 0)
import qualified Data.List.NonEmpty                                      as NE
#endif
data GADTConstructor = GADTConstructor
  { names        :: [WithComments String]
  , forallNeeded :: Bool
  , bindings     :: WithComments (GHC.HsOuterSigTyVarBndrs GHC.GhcPs)
  , con_mb_cxt   :: Maybe (GHC.LHsContext GHC.GhcPs)
  , signature    :: ConstructorSignature
  }

instance CommentExtraction GADTConstructor where
  nodeComments GADTConstructor {} = NodeComments [] [] []

instance Pretty GADTConstructor where
  pretty' (GADTConstructor {..}) = do
    hCommaSep $ fmap (`prettyWith` string) names
    hor <-|> ver
    where
      hor = string " :: " |=> body
      ver = newline >> indentedBlock (string ":: " |=> body)
      body =
        case (forallNeeded, con_mb_cxt) of
          (True, Just _)   -> withForallCtx
          (True, Nothing)  -> withForallOnly
          (False, Just _)  -> withCtxOnly
          (False, Nothing) -> noForallCtx
      withForallCtx = do
        pretty bindings
        (space >> pretty (Context con_mb_cxt)) <-|>
          (newline >> pretty (Context con_mb_cxt))
        newline
        prefixed "=> " $ prettyVertically signature
      withForallOnly = do
        pretty bindings
        (space >> prettyHorizontally signature) <-|>
          (newline >> prettyVertically signature)
      withCtxOnly =
        (pretty (Context con_mb_cxt) >> string " => " >>
         prettyHorizontally signature) <-|>
        (pretty (Context con_mb_cxt) >>
         prefixed "=> " (prettyVertically signature))
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
mkGADTConstructor _ = Nothing

getNames :: GHC.ConDecl GHC.GhcPs -> Maybe [WithComments String]
#if MIN_VERSION_ghc_lib_parser(9, 4, 0)
getNames GHC.ConDeclGADT {..} =
  Just $ NE.toList $ fmap (fmap showOutputable . fromGenLocated) con_names
#else
getNames GHC.ConDeclGADT {..} =
  Just $ fmap (fmap showOutputable . fromGenLocated) con_names
#endif
getNames _ = Nothing
