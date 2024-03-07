{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.TypeSynonym
  ( TypeSynonym
  , mkTypeSynonym
  ) where

import           Control.Monad
import qualified GHC.Types.Fixity                   as GHC
import           HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments
import           HIndent.Pretty.Types

newtype TypeSynonym =
  TypeSynonym (GHC.TyClDecl GHC.GhcPs)

instance CommentExtraction TypeSynonym where
  nodeComments (TypeSynonym _) = NodeComments [] [] []

instance Pretty TypeSynonym where
  pretty' (TypeSynonym GHC.SynDecl {..}) = do
    string "type "
    case tcdFixity of
      GHC.Prefix ->
        spaced $ pretty tcdLName : fmap pretty (GHC.hsq_explicit tcdTyVars)
      GHC.Infix ->
        case GHC.hsq_explicit tcdTyVars of
          (l:r:xs) -> do
            spaced [pretty l, pretty $ fmap InfixOp tcdLName, pretty r]
            forM_ xs $ \x -> do
              space
              pretty x
          _ -> error "Not enough parameters are given."
    hor <-|> ver
    where
      hor = string " = " >> pretty tcdRhs
      ver = newline >> indentedBlock (string "= " |=> pretty tcdRhs)
  pretty' _ = undefined

mkTypeSynonym :: GHC.TyClDecl GHC.GhcPs -> TypeSynonym
mkTypeSynonym = TypeSynonym
