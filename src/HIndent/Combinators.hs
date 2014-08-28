{-# LANGUAGE FlexibleContexts #-}
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
  , comma
  , int
  , string
  -- * Common node types
  , maybeCtx
  -- * Interspersing
  , inter
  , spaced
  , lined
  , prefixedLined
  , commas
  -- * Wrapping
  , parens
  , brackets
  , braces
  -- * Indentation
  , indented
  , column
  , depend
  , swing
  , getIndentSpaces
  -- * Predicates
  , isOverflow
  , isSingleLiner
  , nullBinds
  -- * Sandboxing
  , sandbox
  -- * Fallback
  , pretty'
  )
  where

import           HIndent.Types

import           Control.Monad.State hiding (state)
import           Data.Int
import           Data.List
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as T
import           Data.Text.Lazy.Builder.Int
import qualified Language.Haskell.Exts.Pretty as P
import           Language.Haskell.Exts.Syntax
import           Prelude hiding (exp)

-- | Increase indentation level by n spaces for the given printer.
indented :: Int64 -> Printer a -> Printer a
indented i p =
  do level <- gets psIndentLevel
     modify (\s -> s {psIndentLevel = level + i})
     m <- p
     modify (\s -> s {psIndentLevel = level})
     return m

-- | Print all the printers separated by spaces.
spaced :: [Printer ()] -> Printer ()
spaced = inter space

-- | Print all the printers separated by commas.
commas :: [Printer ()] -> Printer ()
commas = inter comma

-- | Print all the printers separated by sep.
inter :: Printer () -> [Printer ()] -> Printer ()
inter sep ps =
  foldr (\(i,p) next ->
           depend (do p
                      if i <
                         length ps
                         then sep
                         else return ())
                  next)
        (return ())
        (zip [1 ..] ps)

-- | Print all the printers separated by spaces.
lined :: [Printer ()] -> Printer ()
lined ps = sequence_ (intersperse newline ps)

-- | Does printing the given thing overflow column limit? (e.g. 80)
isOverflow :: MonadState PrintState m => m a -> m Bool
isOverflow p =
  do st <- sandbox p
     return (psColumn st >
             columnLimit)

-- | Is the given expression a single-liner when printed?
isSingleLiner :: MonadState PrintState m => m a -> m Bool
isSingleLiner p =
  do line <- gets psLine
     st <- sandbox p
     return (psLine st ==
             line)

-- | Print all the printers separated newlines and optionally a line
-- prefix.
prefixedLined :: Char -> [Printer ()] -> Printer ()
prefixedLined pref ps' =
  case ps' of
    [] -> return ()
    (p:ps) ->
      do p
         indented (-1)
                  (mapM_ (\p' ->
                            do newline
                               depend (string [pref]) p')
                         ps)

-- | Set the (newline-) indent level to the given column for the given
-- printer.
column :: Int64 -> Printer a -> Printer a
column i p =
  do level <- gets psIndentLevel
     modify (\s -> s {psIndentLevel = i})
     m <- p
     modify (\s -> s {psIndentLevel = level})
     return m

-- | Output a newline.
newline :: Printer ()
newline =
  do write "\n"
     modify (\s -> s {psNewline = True})

-- | Make the latter's indentation depend upon the end column of the
-- former.
depend :: Printer () -> Printer b -> Printer b
depend maker dependent =
  do state <- get
     maker
     st <- get
     col <- gets psColumn
     if state /= st
        then column col dependent
        else dependent

-- | Wrap in parens.
parens :: Printer a -> Printer a
parens p =
  depend (write "(")
         (do v <- p
             write ")"
             return v)

-- | Wrap in braces.
braces :: Printer a -> Printer a
braces p =
  depend (write "{")
         (do v <- p
             write "}"
             return v)

-- | Wrap in brackets.
brackets :: Printer a -> Printer a
brackets p =
  depend (write "[")
         (do v <- p
             write "]"
             return v)

-- | Write a space.
space :: Printer ()
space = write " "

-- | Write a comma.
comma :: Printer ()
comma = write ","

-- | Write an integral.
int :: Integral n => n -> Printer ()
int = write . decimal

-- | Write out a string, updating the current position information.
write :: Builder -> Printer ()
write x =
  do state <- get
     let out =
           if psNewline state
              then T.fromText (T.replicate (fromIntegral (psIndentLevel state)) " ") <>
                   x
              else x
         out' = T.toLazyText out
     modify (\s ->
               s {psOutput = psOutput state <> out
                 ,psNewline = False
                 ,psLine = psLine state + additionalLines
                 ,psColumn =
                    if additionalLines > 0
                       then LT.length (LT.concat (take 1 (reverse srclines)))
                       else psColumn state + LT.length out'})
  where x' = T.toLazyText x
        srclines = LT.lines x'
        additionalLines =
          LT.length (LT.filter (== '\n') x')

-- | Pretty print using HSE's own printer. The 'P.Pretty' class here is
-- HSE's.
pretty' :: P.Pretty a => a -> Printer ()
pretty' = write . T.fromText . T.pack . P.prettyPrint

-- | Write a string.
string :: String -> Printer ()
string = write . T.fromText . T.pack

-- | Indent spaces: 2.
getIndentSpaces :: Printer Int64
getIndentSpaces = gets (configIndentSpaces . psConfig)

-- | Column limit: 80
columnLimit :: Int64
columnLimit = 80

-- | Play with a printer and then restore the state to what it was
-- before.
sandbox :: MonadState s m => m a -> m s
sandbox p =
  do orig <- get
     _ <- p
     new <- get
     put orig
     return new

-- | No binds?
nullBinds :: Binds -> Bool
nullBinds (BDecls x) = null x
nullBinds (IPBinds x) = null x

-- | Maybe render a class context.
maybeCtx :: Pretty a => [a] -> Printer ()
maybeCtx ctx =
  unless (null ctx)
         (do write "("
             commas (map pretty ctx)
             write ") => ")

-- | Swing the second printer below and indented with respect to the first.
swing :: Printer () -> Printer b -> Printer b
swing a b =
  do orig <- gets psIndentLevel
     a
     newline
     indentSpaces <- getIndentSpaces
     column (orig + indentSpaces) b
