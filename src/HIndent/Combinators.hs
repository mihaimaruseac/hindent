{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Combinators used for printing.

module HIndent.Combinators
  (
  -- * Insertion
    write
  , newline
  , space
  , int
  -- * Indentation
  , indented
  , column
  , depend
  -- * Fallback printer
  , pretty'
  )
  where

import           HIndent.Types

import           Control.Monad.State hiding (state)
import           Data.Int
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as T
import           Data.Text.Lazy.Builder.Int
import qualified Language.Haskell.Exts.Pretty as P

-- | Increase indentation level by n spaces for the given printer.
indented :: Int64 -> Printer a -> Printer a
indented i p =
  do level <- gets psIndentLevel
     modify (\s -> s { psIndentLevel = level + i })
     m <- p
     modify (\s -> s { psIndentLevel = level })
     return m

-- | Set the (newline-) indent level to the given column for the given
-- printer.
column :: Int64 -> Printer a -> Printer a
column i p =
  do level <- gets psIndentLevel
     modify (\s -> s { psIndentLevel = i })
     m <- p
     modify (\s -> s { psIndentLevel = level })
     return m

-- | Output a newline.
newline :: Printer ()
newline = do
  write "\n"
  modify (\s -> s { psNewline = True })

-- | Make the latter's indentation depend upon the end column of the
-- former.
depend :: Printer () -> Printer b -> Printer b
depend maker dependent =
  do maker
     col <- gets psColumn
     column col dependent

-- | Write a space.
space :: Printer ()
space = write " "

-- | Write an integral.
int :: Integral n => n -> Printer ()
int = write . decimal

-- | Write out a string, updating the current position information.
write :: Builder -> Printer ()
write x = do
  state <- get
  let out =
        if psNewline state
           then T.fromText (T.replicate (fromIntegral (psIndentLevel state)) " ") <> x
           else x
      out' = T.toLazyText out
  modify
    (\s ->
       s { psOutput  = psOutput state <> out
         , psNewline = False
         , psColumn  =
             if additionalLines > 0
                then LT.length (LT.concat (take 1 (reverse srclines)))
                else psColumn state + LT.length out'
         })
  where
        x' = T.toLazyText x
        srclines = LT.lines x'
        additionalLines = LT.length (LT.filter (=='\n') x')

-- | Pretty print using HSE's own printer. The 'P.Pretty' class here is
-- HSE's.
pretty' :: P.Pretty a => a -> Printer ()
pretty' = write . T.fromText . T.pack . P.prettyPrint
