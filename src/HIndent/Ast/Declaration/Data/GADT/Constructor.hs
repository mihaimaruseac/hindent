{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Data.GADT.Constructor
  ( GADTConstructor(..)
  , mkGADTConstructor
  ) where

import Data.Maybe
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.Declaration.Data.GADT.Constructor.Signature
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 6, 0)
import qualified Data.List.NonEmpty as NE
#endif
data GADTConstructor = GADTConstructor
  { names :: [WithComments String]
  , forallNeeded :: Bool
  , bindings :: WithComments (GHC.HsOuterSigTyVarBndrs GHC.GhcPs)
  , con_mb_cxt :: Maybe (GHC.LHsContext GHC.GhcPs)
  , signature :: ConstructorSignature
  }

instance CommentExtraction GADTConstructor where
  nodeComments GADTConstructor {} = NodeComments [] [] []

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
#if MIN_VERSION_ghc_lib_parser(9, 6, 0)
getNames GHC.ConDeclGADT {..} =
  Just $ NE.toList $ fmap (fmap showOutputable . fromGenLocated) con_names
#else
getNames GHC.ConDeclGADT {..} =
  Just $ fmap (fmap showOutputable . fromGenLocated) con_names
#endif
getNames _ = Nothing
