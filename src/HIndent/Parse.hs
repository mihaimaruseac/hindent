{-# LANGUAGE CPP #-}

-- | Parsing and lexical analysis functions.
module HIndent.Parse
  ( parseModule
  , lexCode
  ) where

import Data.Maybe
import qualified GHC.Data.EnumSet as ES
import GHC.Data.FastString
import GHC.Data.StringBuffer
import qualified GHC.LanguageExtensions as GLP
import qualified GHC.Parser as GLP
import GHC.Parser.Lexer hiding (buffer)
import GHC.Stack
import GHC.Types.SrcLoc
import HIndent.GhcLibParserWrapper.GHC.Hs
#if MIN_VERSION_ghc_lib_parser(9,4,1)
import GHC.Utils.Error
import GHC.Utils.Outputable hiding ((<>), empty, text)
#endif
#if MIN_VERSION_ghc_lib_parser(9,8,1)
import GHC.Unit.Module.Warnings
#endif

-- | This function parses the given Haskell source code with the given file
-- path (if any) and parse options.
parseModule ::
     Maybe FilePath -> [GLP.Extension] -> String -> ParseResult HsModule'
parseModule filepath exts src =
  case unP GLP.parseModule initState of
    POk s m -> POk s $ unLoc m
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
--
-- Note: We currently support GHC 8.10.7 and newer and this implies we use
-- `ghc-lib-parser` from 9.2.8 upwards (due to it supporting back two GHC
-- versions). Due to API changes, we need to use preprocessor macros, as
-- described here:
--
--   - For GHC 8.10, ghc-lib-parser-9.2.8 has one extra argument as first
--     argument of `mkParserOpts`. All newer versions of ghc-lib-parser have
--     2 additional arguments as second and third argument.
--   - For GHC 9.4 and newer, ghc-lib-parser-9.8.1 has 2 more strict fields in
--     the constructor for `DiagOpts`.
parserOptsFromExtensions :: [GLP.Extension] -> ParserOpts
parserOptsFromExtensions opts =
  mkParserOpts
#if MIN_VERSION_ghc_lib_parser(9,4,1)
#else
    -- TODO(mihaimaruseac): Remove when dropping GHC 8.10 support
    ES.empty -- No compiler warnings are enabled
#endif
    opts'
#if MIN_VERSION_ghc_lib_parser(9,4,1)
    diagOpts
    [] -- There are no supported languages and extensions (used only in errors)
#endif
    False -- Safe imports are off.
    False -- Haddock comments are treated as normal comments.
    True -- Comments are kept in an AST.
    False -- Do not update the internal position of a comment.
  where
    opts' = ES.fromList $ GLP.StarIsType : opts
#if MIN_VERSION_ghc_lib_parser(9,4,1)
    diagOpts =
      DiagOpts
        { diag_warning_flags = ES.empty
        , diag_fatal_warning_flags = ES.empty
#if MIN_VERSION_ghc_lib_parser(9,8,1)
        , diag_custom_warning_categories = emptyWarningCategorySet
        , diag_fatal_custom_warning_categories = emptyWarningCategorySet
#endif
        , diag_warn_is_error = False
        , diag_reverse_errors = False
        , diag_max_errors = Nothing
        , diag_ppr_ctx = defaultSDocContext
        }
#endif
