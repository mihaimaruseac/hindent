{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module HIndent.Ast.Declaration.Data.Body
  ( DataBody(..)
  , mkDataBody
  ) where

import Data.Maybe
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.Declaration.Data.GADT.Constructor
import HIndent.Ast.NodeComments
import HIndent.Ast.Type
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty.NodeComments

data DataBody
  = GADT
      { kind :: Maybe (WithComments Type)
      , constructors :: [WithComments GADTConstructor]
      }
  | Record
      { dd_cons :: [GHC.LConDecl GHC.GhcPs]
      , dd_derivs :: GHC.HsDeriving GHC.GhcPs
      }

instance CommentExtraction DataBody where
  nodeComments GADT {} = NodeComments [] [] []
  nodeComments Record {} = NodeComments [] [] []

mkDataBody :: GHC.HsDataDefn GHC.GhcPs -> DataBody
mkDataBody defn@GHC.HsDataDefn {..} =
  if isGADT defn
    then GADT
           { constructors =
               fromMaybe (error "Some constructors are not GADT ones.")
                 $ mapM (traverse mkGADTConstructor . fromGenLocated)
                 $ getConDecls defn
           , ..
           }
    else Record {dd_cons = getConDecls defn, ..}
  where
    kind = fmap mkType . fromGenLocated <$> dd_kindSig

isGADT :: GHC.HsDataDefn GHC.GhcPs -> Bool
isGADT (getConDecls -> (GHC.L _ GHC.ConDeclGADT {}:_)) = True
isGADT _ = False

getConDecls :: GHC.HsDataDefn GHC.GhcPs -> [GHC.LConDecl GHC.GhcPs]
#if MIN_VERSION_ghc_lib_parser(9, 6, 0)
getConDecls GHC.HsDataDefn {..} =
  case dd_cons of
    GHC.NewTypeCon x -> [x]
    GHC.DataTypeCons _ xs -> xs
#else
getConDecls GHC.HsDataDefn {..} = dd_cons
#endif
