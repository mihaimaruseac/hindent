{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.TypeSynonym
  ( TypeSynonym
  , mkTypeSynonym
  ) where

import qualified GHC.Types.Fixity                   as GHC
import           HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.NodeComments
import           HIndent.Pretty.Types

data TypeSynonym = TypeSynonym
  { lhs     :: Lhs
  , synonym :: GHC.TyClDecl GHC.GhcPs
  , rhs     :: GHC.LHsType GHC.GhcPs
  }

data Lhs
  = Prefix
      { name          :: GHC.LIdP GHC.GhcPs
      , typeVariables :: [GHC.LHsTyVarBndr () GHC.GhcPs]
      }
  | Infix
      { left  :: GHC.LHsTyVarBndr () GHC.GhcPs
      , name  :: GHC.LIdP GHC.GhcPs
      , right :: GHC.LHsTyVarBndr () GHC.GhcPs
      }

instance CommentExtraction TypeSynonym where
  nodeComments TypeSynonym {} = NodeComments [] [] []

instance Pretty TypeSynonym where
  pretty' TypeSynonym {synonym = GHC.SynDecl {..}, ..} = do
    string "type "
    case lhs of
      Prefix {..} -> spaced $ pretty name : fmap pretty typeVariables
      Infix {..} ->
        spaced [pretty left, pretty $ fmap InfixOp tcdLName, pretty right]
    hor <-|> ver
    where
      hor = string " = " >> pretty rhs
      ver = newline >> indentedBlock (string "= " |=> pretty rhs)
  pretty' _ = undefined

mkTypeSynonym :: GHC.TyClDecl GHC.GhcPs -> TypeSynonym
mkTypeSynonym synonym@GHC.SynDecl {..} = TypeSynonym {..}
  where
    lhs =
      case tcdFixity of
        GHC.Prefix ->
          Prefix {name = tcdLName, typeVariables = GHC.hsq_explicit tcdTyVars}
        GHC.Infix ->
          case GHC.hsq_explicit tcdTyVars of
            [l, r] -> Infix {left = l, name = tcdLName, right = r}
            _ ->
              error
                "Unexpected number of type variables for infix type synonym."
    rhs = tcdRhs
mkTypeSynonym _ = error "Not a type synonym."
