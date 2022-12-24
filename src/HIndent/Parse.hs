{-# LANGUAGE CPP #-}

-- | Parsing and lexical analysis functions.
module HIndent.Parse
  ( parseModule
  , lexCode
  ) where

import           Data.Maybe
import qualified GHC.Data.EnumSet       as ES
import           GHC.Data.FastString
import           GHC.Data.StringBuffer
import           GHC.Hs
import qualified GHC.LanguageExtensions as GLP
import qualified GHC.Parser             as GLP
import           GHC.Parser.Lexer       hiding (buffer)
import           GHC.Stack
import           GHC.Types.SrcLoc
#if MIN_VERSION_ghc_lib_parser(9,4,1)
import           GHC.Utils.Error
import           GHC.Utils.Outputable   hiding (empty, text, (<>))
#endif
-- | This function parses the given Haskell source code with the given file
-- path (if any) and parse options.
parseModule ::
     Maybe FilePath -> [GLP.Extension] -> String -> ParseResult HsModule
parseModule filepath exts src =
  case unP GLP.parseModule initState of
    POk s m   -> POk s $ unLoc m
    PFailed s -> PFailed s
  where
    initState = initParserState (parserOptsFromExtensions exts) buffer location
    location =
      mkRealSrcLoc (mkFastString $ fromMaybe "<interactive>" filepath) 1 1
    buffer = stringToStringBuffer src

-- | Lexically analyze the given code.
lexCode :: HasCallStack => String -> [Token]
lexCode code
  | POk _ tokens <-
      lexTokenStream
        (parserOptsFromExtensions [])
        (stringToStringBuffer code)
        (mkRealSrcLoc (mkFastString "<interactive>") 1 1) = fmap unLoc tokens
  | otherwise = error "Failed to lex the code."

-- | This function generates a 'ParserOpts' from te given extension.
--
-- The 'StarIsType' extension is always enabled to compile a code using
-- kinds like '* -> *'.
parserOptsFromExtensions :: [GLP.Extension] -> ParserOpts
#if MIN_VERSION_ghc_lib_parser(9,4,1)
parserOptsFromExtensions opts =
  mkParserOpts
    opts'
    diagOpts
    [] -- There are no supported languages and extensions (this list is used only in error messages)
    False -- Safe imports are off.
    False -- Haddock comments are treated as normal comments.
    True -- Comments are kept in an AST.
    False -- Do not update the internal position of a comment.
  where
    opts' = ES.fromList $ GLP.StarIsType : opts
    diagOpts =
      DiagOpts
        { diag_warning_flags = ES.empty
        , diag_fatal_warning_flags = ES.empty
        , diag_warn_is_error = False
        , diag_reverse_errors = False
        , diag_max_errors = Nothing
        , diag_ppr_ctx = defaultSDocContext
        }
#else
parserOptsFromExtensions opts =
  mkParserOpts
    ES.empty -- No compiler warnings are enabled.
    opts'
    False -- Safe imports are off.
    False -- Haddock comments are treated as normal comments.
    True -- Comments are kept in an AST.
    False -- Do not update the internal position of a comment.
  where
    opts' = ES.fromList $ GLP.StarIsType : opts
#endif
