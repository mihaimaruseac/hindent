{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Data.Haskell98.Constructor.Body
  ( Haskell98ConstructorBody
  , mkHaskell98ConstructorBody
  , isRecord
  ) where

import HIndent.Ast.Declaration.Data.Constructor.Field
import HIndent.Ast.Declaration.Data.Record.Field
import HIndent.Ast.Name.Infix
import HIndent.Ast.Name.Prefix
import HIndent.Ast.NodeComments
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
data Haskell98ConstructorBody
  = Infix
      { iName :: WithComments InfixName -- Using `name` in all constructors causes a type clash
      , left :: ConstructorField
      , right :: ConstructorField
      }
  | Prefix
      { pName :: WithComments PrefixName
      , types :: [ConstructorField]
      }
  | Record
      { rName :: WithComments PrefixName
      , records :: WithComments [WithComments RecordField]
      }

instance CommentExtraction Haskell98ConstructorBody where
  nodeComments Infix {} = NodeComments [] [] []
  nodeComments Prefix {} = NodeComments [] [] []
  nodeComments Record {} = NodeComments [] [] []

instance Pretty Haskell98ConstructorBody where
  pretty' Infix {..} =
    spaced [pretty left, pretty iName, pretty right]
  pretty' Prefix {..} = pretty pName >> hor <-|> ver
    where
      hor = spacePrefixed $ fmap pretty types
      ver = indentedBlock $ newlinePrefixed $ fmap pretty types
  pretty' Record {..} = do
    pretty rName
    prettyWith records $ \r ->
      newline >> indentedBlock (vFields $ fmap pretty r)

mkHaskell98ConstructorBody ::
     GHC.ConDecl GHC.GhcPs -> Maybe Haskell98ConstructorBody
mkHaskell98ConstructorBody GHC.ConDeclH98 { con_args = GHC.InfixCon leftField rightField
                                          , ..
                                          } = Just Infix {..}
  where
    iName = fromGenLocated $ fmap mkInfixName con_name
    left = mkConstructorField leftField
    right = mkConstructorField rightField
#if MIN_VERSION_ghc_lib_parser(9, 14, 0)
mkHaskell98ConstructorBody GHC.ConDeclH98 { con_args = GHC.PrefixCon rawTypes
                                          , ..
                                          } = Just Prefix {..}
  where
    pName = fromGenLocated $ fmap mkPrefixName con_name
    types = fmap mkConstructorField rawTypes
#else
mkHaskell98ConstructorBody GHC.ConDeclH98 { con_args = GHC.PrefixCon _ rawTypes
                                          , ..
                                          } = Just Prefix {..}
  where
    pName = fromGenLocated $ fmap mkPrefixName con_name
    types = fmap mkConstructorField rawTypes
#endif
mkHaskell98ConstructorBody GHC.ConDeclH98 {con_args = GHC.RecCon rs, ..} =
  Just Record {..}
  where
    rName = fromGenLocated $ fmap mkPrefixName con_name
    records =
      fromGenLocated $ fmap (fmap (fmap mkRecordField . fromGenLocated)) rs
mkHaskell98ConstructorBody GHC.ConDeclGADT {} = Nothing

isRecord :: Haskell98ConstructorBody -> Bool
isRecord Record {} = True
isRecord _ = False
