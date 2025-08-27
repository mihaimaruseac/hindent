module HIndent.Ast.Type.Literal
  ( Literal
  , mkLiteral
  ) where

import qualified GHC.Data.FastString as GHC
import HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments

data Literal
  = Numeric Integer
  | String String
  | Character Char

instance CommentExtraction Literal where
  nodeComments _ = NodeComments [] [] []

instance Pretty Literal where
  pretty' (Numeric n) = string $ show n
  pretty' (String s) = string $ formatStringLiteral s
  pretty' (Character c) = string $ formatCharLiteral c

mkLiteral :: GHC.HsTyLit GHC.GhcPs -> Literal
mkLiteral (GHC.HsNumTy _ n) = Numeric n
mkLiteral (GHC.HsStrTy _ s) = String (GHC.unpackFS s)
mkLiteral (GHC.HsCharTy _ c) = Character c

formatStringLiteral :: String -> String
formatStringLiteral s = "\"" ++ concatMap (escapeCharForLiteral '"') s ++ "\""

formatCharLiteral :: Char -> String
formatCharLiteral c = "'" ++ escapeCharForLiteral '\'' c ++ "'"

escapeCharForLiteral :: Char -> Char -> String
escapeCharForLiteral quoteChar c
  | c == quoteChar = ['\\', quoteChar]
  | c == '\\' = "\\\\"
  | c == '\n' = "\\n"
  | c == '\t' = "\\t"
  | c == '\r' = "\\r"
  | c `elem` ['\0' .. '\8'] ++ ['\11' .. '\12'] ++ ['\14' .. '\31'] =
    '\\' : show (fromEnum c)
  | otherwise = [c]
