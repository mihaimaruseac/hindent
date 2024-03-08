{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Data.GADT.Constructor
  ( GADTConstructor
  , mkGADTConstructor
  ) where

import           Data.Maybe
import qualified GHC.Types.SrcLoc                   as GHC
import           HIndent.Ast.NodeComments
import           HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments
import           HIndent.Pretty.Types
#if MIN_VERSION_ghc_lib_parser(9, 4, 0)
import qualified Data.List.NonEmpty                 as NE
#endif
data GADTConstructor = GADTConstructor
  { names        :: [WithComments String]
  , forallNeeded :: Bool
  , con_bndrs    :: WithComments (GHC.HsOuterSigTyVarBndrs GHC.GhcPs)
  , con_mb_cxt   :: Maybe (GHC.LHsContext GHC.GhcPs)
  , con_res_ty   :: GHC.LHsType GHC.GhcPs
  , con_g_args   :: GHC.HsConDeclGADTDetails GHC.GhcPs
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
        pretty con_bndrs
        (space >> pretty (Context con_mb_cxt)) <-|>
          (newline >> pretty (Context con_mb_cxt))
        newline
        prefixed "=> " verArgs
      withForallOnly = do
        pretty con_bndrs
        (space >> horArgs) <-|> (newline >> verArgs)
      withCtxOnly =
        (pretty (Context con_mb_cxt) >> string " => " >> horArgs) <-|>
        (pretty (Context con_mb_cxt) >> prefixed "=> " verArgs)
      noForallCtx = horArgs <-|> verArgs
      horArgs = printArgsBy $ inter (string " -> ")
      verArgs = printArgsBy $ prefixedLined "-> "
      printArgsBy f =
        case con_g_args of
          GHC.PrefixConGADT xs -> f $ fmap pretty xs ++ [pretty con_res_ty]
          GHC.RecConGADT xs    -> f [recArg xs, pretty con_res_ty]
      recArg xs = printCommentsAnd xs $ \xs' -> vFields' $ fmap pretty xs'

mkGADTConstructor :: GHC.ConDecl GHC.GhcPs -> Maybe GADTConstructor
mkGADTConstructor decl@GHC.ConDeclGADT {..} =
  Just $ GADTConstructor {con_bndrs = fromGenLocated con_bndrs, ..}
  where
    names = fromMaybe (error "Couldn't get names.") $ getNames decl
    forallNeeded =
      case GHC.unLoc con_bndrs of
        GHC.HsOuterImplicit {} -> False
        GHC.HsOuterExplicit {} -> True
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
