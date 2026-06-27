{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Printer combinators related to print strings.
module HIndent.Pretty.Combinators.String
  ( string
  , space
  , newline
  , blankline
  , comma
  , dot
  ) where

import Control.Monad.RWS
import qualified Data.ByteString.Builder as S
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Stack
import HIndent.Config
import HIndent.Printer
#if MIN_VERSION_GLASGOW_HASKELL(9,6,0,0)
import Control.Monad
#endif
-- | This function prints the given string.
--
-- The string must not include @\n@s. Use @newline@ to print them.
string :: HasCallStack => Text.Text -> Printer ()
string x
  | "\n" `Text.isInfixOf` x =
    error
      $ "You tried to print " ++ show x ++ ". Use `newline` to print '\\n's."
  | otherwise = do
    eol <- gets psEolComment
    hardFail <- gets psFitOnOneLine
    when eol newline
    st <- get
    let indentSpaces =
          if psNewline st
            then Text.replicate (psIndentLevel st) " "
            else ""
        out = indentSpaces <> x
        psColumn' = psColumn st + Text.length out
        columnFits = psColumn' <= configMaxColumns (psConfig st)
    when hardFail $ guard columnFits
    modify
      (\s ->
         s
           { psOutput = psOutput st <> S.byteString (Text.encodeUtf8 out)
           , psNewline = False
           , psEolComment = False
           , psColumn = psColumn'
           })

-- | Equivalent to @string " "@.
space :: Printer ()
space = string " "

-- | Equivalent to @string ","@.
comma :: Printer ()
comma = string ","

-- | Equivalent to @string "."@.
dot :: Printer ()
dot = string "."

-- | This function prints a @\n@.
--
-- Always call this function to print it because printing it requires
-- special treatment. Do not call @string@ instead.
newline :: Printer ()
newline = do
  gets psFitOnOneLine >>= guard . not
  modify
    (\s ->
       s
         { psOutput = psOutput s <> S.stringUtf8 "\n"
         , psNewline = True
         , psLine = psLine s + 1
         , psEolComment = False
         , psColumn = 0
         })

-- | Equivalent to @newline >> newline@.
blankline :: Printer ()
blankline = newline >> newline
