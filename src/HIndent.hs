{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings, TupleSections, ScopedTypeVariables #-}

-- | Haskell indenter.

module HIndent
  (-- * Formatting functions.
   reformat
  ,prettyPrint
  ,parseMode
  -- * Style
  ,Style(..)
  ,styles
  ,chrisDone
  ,johanTibell
  ,fundamental
  ,gibiansky
  -- * Testing
  ,test
  ,testAll
  ,testAst
  )
  where

import           HIndent.Comments
import           HIndent.Pretty
import           HIndent.Styles.ChrisDone (chrisDone)
import           HIndent.Styles.Fundamental (fundamental)
import           HIndent.Styles.Gibiansky (gibiansky)
import           HIndent.Styles.JohanTibell (johanTibell)
import           HIndent.Types

import           Control.Monad.State.Strict
import           Control.Monad.Trans.Maybe
import           Data.Functor.Identity
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import qualified Data.Text.IO as ST
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.IO as T
import           Language.Haskell.Exts.Annotated hiding (Style,prettyPrint,Pretty,style,parse)

-- | Format the given source.
reformat :: Style -> Maybe [Extension] -> Text -> Either String Builder
reformat style mexts x =
  case parseModuleWithComments mode
                               (T.unpack x) of
    ParseOk (m,comments) ->
      let (cs,ast) =
            annotateComments (fromMaybe m $ applyFixities baseFixities m) comments
      in Right (prettyPrint
                  mode
                  style
                  -- For the time being, assume that all "free-floating" comments come at the beginning.
                  -- If they were not at the beginning, they would be after some ast node.
                  -- Thus, print them before going for the ast.
                  (do mapM_ (printComment Nothing) (reverse cs)
                      pretty ast))
    ParseFailed _ e -> Left e
  where mode = (case mexts of
                  Just exts -> parseMode {extensions = exts}
                  Nothing -> parseMode)

-- | Pretty print the given printable thing.
prettyPrint :: ParseMode -> Style -> (forall s. Printer s ()) -> Builder
prettyPrint mode style m =
  case style of
    Style _name _author _desc st extenders config ->
      maybe (error "Printer failed with mzero call.")
            psOutput
            (runIdentity
               (runMaybeT (execStateT (runPrinter m)
                                      (PrintState 0 mempty False 0 1 st extenders config False False mode))))

-- | Parse mode, includes all extensions, doesn't assume any fixities.
parseMode :: ParseMode
parseMode =
  defaultParseMode {extensions = allExtensions
                   ,fixities = Nothing}
  where allExtensions =
          filter isDisabledExtention knownExtensions
        isDisabledExtention (DisableExtension _) = False
        isDisabledExtention _ = True

-- | Test with the given style, prints to stdout.
test :: Style -> Text -> IO ()
test style =
  either error (T.putStrLn . T.toLazyText) .
  reformat style Nothing

-- | Test with all styles, prints to stdout.
testAll :: Text -> IO ()
testAll i =
  forM_ styles
        (\style ->
           do ST.putStrLn ("-- " <> styleName style <> ":")
              test style i
              ST.putStrLn "")

-- | Parse the source and annotate it with comments, yielding the resulting AST.
testAst :: Text -> Either String ([ComInfo], Module NodeInfo)
testAst x =
  case parseModuleWithComments parseMode
                               (T.unpack x) of
    ParseOk (m,comments) ->
      Right (annotateComments m comments)
    ParseFailed _ e -> Left e

-- | Styles list, useful for programmatically choosing.
styles :: [Style]
styles =
  [fundamental,chrisDone,johanTibell,gibiansky]
