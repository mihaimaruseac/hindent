{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module HIndent.Ast.Declaration.Data.Body
  ( DataBody
  , mkDataBody
  ) where

import Control.Monad
import Data.Maybe
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Applicative
import HIndent.Ast.Declaration.Data.GADT.Constructor
import HIndent.Ast.Declaration.Data.Haskell98.Constructor
import HIndent.Ast.NodeComments
import HIndent.Ast.Type
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data DataBody
  = GADT
      { kind :: Maybe (WithComments Type)
      , constructors :: [WithComments GADTConstructor]
      }
  | Haskell98
      { dd_cons :: [WithComments Haskell98Constructor]
      , dd_derivs :: GHC.HsDeriving GHC.GhcPs
      }

instance CommentExtraction DataBody where
  nodeComments GADT {} = NodeComments [] [] []
  nodeComments Haskell98 {} = NodeComments [] [] []

instance Pretty DataBody where
  pretty' GADT {..} = do
    whenJust kind $ \x -> string " :: " >> pretty x
    string " where"
    indentedBlock $ newlinePrefixed $ fmap pretty constructors
  pretty' Haskell98 {..} = do
    case dd_cons of
      [] -> indentedBlock derivingsAfterNewline
      [x]
        | hasSingleRecordConstructor $ getNode x -> do
          string " = "
          pretty x
          unless (null dd_derivs) $ space |=> printDerivings
      [x] -> do
        string " ="
        newline
        indentedBlock $ pretty x >> derivingsAfterNewline
      _ ->
        indentedBlock $ do
          newline
          string "= " |=> vBarSep (fmap pretty dd_cons)
          derivingsAfterNewline
    where
      derivingsAfterNewline =
        unless (null dd_derivs) $ newline >> printDerivings
      printDerivings = lined $ fmap pretty dd_derivs

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
    else Haskell98
           { dd_cons =
               fmap
                 (fmap (fromMaybe undefined . mkHaskell98Constructor)
                    . fromGenLocated)
                 $ getConDecls defn
           , ..
           }
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
