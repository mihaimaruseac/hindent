{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Data.GADT
  ( GADTConstructor
  , mkGADTConstructor
  ) where

import qualified GHC.Types.SrcLoc                   as GHC
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments
import           HIndent.Pretty.Types

newtype GADTConstructor = GADTConstructor
  { constructor :: GHC.ConDecl GHC.GhcPs
  }

instance CommentExtraction GADTConstructor where
  nodeComments (GADTConstructor x) = nodeComments x

instance Pretty GADTConstructor where
  pretty' (GADTConstructor {constructor = GHC.ConDeclGADT {..}}) = do
    hCommaSep $ fmap pretty con_names
    hor <-|> ver
    where
      hor = string " :: " |=> body
      ver = newline >> indentedBlock (string ":: " |=> body)
      body =
        case (forallNeeded, con_mb_cxt) of
          (True, Just ctx)  -> withForallCtx ctx
          (True, Nothing)   -> withForallOnly
          (False, Just ctx) -> withCtxOnly ctx
          (False, Nothing)  -> noForallCtx
      withForallOnly = do
        pretty con_bndrs
        (space >> horArgs) <-|> (newline >> verArgs)
      noForallCtx = horArgs <-|> verArgs
      withForallCtx _ = do
        pretty con_bndrs
        (space >> pretty (Context con_mb_cxt)) <-|>
          (newline >> pretty (Context con_mb_cxt))
        newline
        prefixed "=> " verArgs
      withCtxOnly _ =
        (pretty (Context con_mb_cxt) >> string " => " >> horArgs) <-|>
        (pretty (Context con_mb_cxt) >> prefixed "=> " verArgs)
      horArgs =
        case con_g_args of
          GHC.PrefixConGADT xs ->
            inter (string " -> ") $
            fmap (\(GHC.HsScaled _ x) -> pretty x) xs ++ [pretty con_res_ty]
          GHC.RecConGADT xs ->
            inter (string " -> ") [recArg xs, pretty con_res_ty]
      verArgs =
        case con_g_args of
          GHC.PrefixConGADT xs ->
            prefixedLined "-> " $
            fmap (\(GHC.HsScaled _ x) -> pretty x) xs ++ [pretty con_res_ty]
          GHC.RecConGADT xs ->
            prefixedLined "-> " [recArg xs, pretty con_res_ty]
      recArg xs = printCommentsAnd xs $ \xs' -> vFields' $ fmap pretty xs'
      forallNeeded =
        case GHC.unLoc con_bndrs of
          GHC.HsOuterImplicit {} -> False
          GHC.HsOuterExplicit {} -> True
  pretty' _ = error "Not a GADT constructor."

mkGADTConstructor :: GHC.ConDecl GHC.GhcPs -> Maybe GADTConstructor
mkGADTConstructor x@GHC.ConDeclGADT {} = Just $ GADTConstructor x
mkGADTConstructor _                    = Nothing
