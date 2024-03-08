{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Data.GADT.Constructor
  ( GADTConstructor
  , mkGADTConstructor
  ) where

import qualified GHC.Types.SrcLoc                   as GHC
import           HIndent.Ast.NodeComments
import           HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments
import           HIndent.Pretty.Types

data GADTConstructor = GADTConstructor
  { names        :: [WithComments String]
  , forallNeeded :: Bool
  , constructor  :: GHC.ConDecl GHC.GhcPs
  }

instance CommentExtraction GADTConstructor where
  nodeComments GADTConstructor {} = NodeComments [] [] []

instance Pretty GADTConstructor where
  pretty' (GADTConstructor {constructor = GHC.ConDeclGADT {..}, ..}) = do
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
  pretty' _ = error "Not a GADT constructor."

mkGADTConstructor :: GHC.ConDecl GHC.GhcPs -> Maybe GADTConstructor
mkGADTConstructor constructor@GHC.ConDeclGADT {..} = Just $ GADTConstructor {..}
  where
    names = fmap (fmap showOutputable . fromGenLocated) con_names
    forallNeeded =
      case GHC.unLoc con_bndrs of
        GHC.HsOuterImplicit {} -> False
        GHC.HsOuterExplicit {} -> True
mkGADTConstructor _ = Nothing
