{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Data.Haskell98.Constructor.Body
  ( Haskell98ConstructorBody
  , mkHaskell98ConstructorBody
  , isRecord
  ) where

import HIndent.Ast.Declaration.Data.Record.Field
import HIndent.Ast.Name.Infix
import HIndent.Ast.Name.Prefix
import HIndent.Ast.NodeComments
#if MIN_VERSION_ghc_lib_parser(9, 14, 0)
import HIndent.Ast.Type (mkTypeFromConDeclField)
#endif
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Printer (Printer)
#if MIN_VERSION_ghc_lib_parser(9, 14, 0)
type ConstructorField = GHC.HsConDeclField GHC.GhcPs
#else
type ConstructorField = GHC.HsScaled GHC.GhcPs (GHC.LBangType GHC.GhcPs)
#endif
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

prettyConstructorField :: ConstructorField -> Printer ()
#if MIN_VERSION_ghc_lib_parser(9, 14, 0)
prettyConstructorField field = pretty (mkTypeFromConDeclField field)
#else
prettyConstructorField = pretty
#endif
instance Pretty Haskell98ConstructorBody where
  pretty' Infix {..} =
    spaced
      [prettyConstructorField left, pretty iName, prettyConstructorField right]
  pretty' Prefix {..} = pretty pName >> hor <-|> ver
    where
      hor = spacePrefixed $ fmap prettyConstructorField types
      ver = indentedBlock $ newlinePrefixed $ fmap prettyConstructorField types
  pretty' Record {..} = do
    pretty rName
    prettyWith records $ \r ->
      newline >> indentedBlock (vFields $ fmap pretty r)

mkHaskell98ConstructorBody ::
     GHC.ConDecl GHC.GhcPs -> Maybe Haskell98ConstructorBody
mkHaskell98ConstructorBody GHC.ConDeclH98 { con_args = GHC.InfixCon left right
                                          , ..
                                          } = Just Infix {..}
  where
    iName = fromGenLocated $ fmap mkInfixName con_name
#if MIN_VERSION_ghc_lib_parser(9, 14, 0)
mkHaskell98ConstructorBody GHC.ConDeclH98 {con_args = GHC.PrefixCon types, ..} =
  Just Prefix {..}
  where
    pName = fromGenLocated $ fmap mkPrefixName con_name
#else
mkHaskell98ConstructorBody GHC.ConDeclH98 {con_args = GHC.PrefixCon _ types, ..} =
  Just Prefix {..}
  where
    pName = fromGenLocated $ fmap mkPrefixName con_name
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
