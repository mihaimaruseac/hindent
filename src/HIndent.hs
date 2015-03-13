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
import qualified Data.Text.Lazy as T hiding (singleton)
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.IO as T
import           Language.Haskell.Exts.Annotated hiding (Style, prettyPrint, Pretty, style, parse)
import           Data.Function (on)
import           Data.List (groupBy, intersperse)
import           Control.Applicative ((<$>))

data CodeBlock = HaskellSource Text
               | CPPDirectives Text
  deriving (Show, Eq)

-- | Format the given source.
reformat :: Style -> Maybe [Extension] -> Text -> Either String Builder
reformat style mexts x = mconcat <$> intersperse (T.singleton '\n') <$> mapM processBlock (cppSplitBlocks x)
  where
    processBlock :: CodeBlock -> Either String Builder
    processBlock (CPPDirectives text) = Right $ T.fromLazyText text
    processBlock (HaskellSource text) =
      case parseModuleWithComments mode' (T.unpack text) of
        ParseOk (m,comments) -> prettyPrint mode' style m comments
        ParseFailed _ e -> Left e

    mode' =
      case mexts of
        Just exts -> parseMode {extensions = exts}
        Nothing -> parseMode

-- | Break a Haskell code string into chunks, using CPP as a delimiter.
-- Lines that start with '#if', '#end', or '#else' are their own chunks, and
-- also act as chunk separators. For example, the code
--
-- > #ifdef X
-- > x = X
-- > y = Y
-- > #else
-- > x = Y
-- > y = X
-- > #endif
--
-- will become five blocks, one for each CPP line and one for each pair of declarations.
cppSplitBlocks :: Text -> [CodeBlock]
cppSplitBlocks inp =
  modifyLast (inBlock (`T.append` trailing)) .
  map (classify . mconcat . intersperse "\n") .
  groupBy ((==) `on` cppLine) .
  T.lines $ inp
  where
    cppLine :: Text -> Bool
    cppLine src = any (`T.isPrefixOf` src) ["#if", "#end", "#else", "#define", "#undef"]

    classify :: Text -> CodeBlock
    classify text = if cppLine text
                    then CPPDirectives text
                    else HaskellSource text

    -- Hack to work around some parser issues in haskell-src-exts: Some pragmas
    -- need to have a newline following them in order to parse properly, so we include
    -- the trailing newline in the code block if it existed.
    trailing :: Text
    trailing = if T.isSuffixOf "\n" inp then "\n" else ""

    modifyLast :: (a -> a) -> [a] -> [a]
    modifyLast _ []  = []
    modifyLast f [x] = [f x]
    modifyLast f (x:xs) = x : modifyLast f xs

    inBlock :: (Text -> Text) -> CodeBlock -> CodeBlock
    inBlock f (HaskellSource txt) = HaskellSource (f txt)
    inBlock _ dir = dir

-- | Print the module.
prettyPrint :: ParseMode
            -> Style
            -> Module SrcSpanInfo
            -> [Comment]
            -> Either a Builder
prettyPrint mode' style m comments =
  let (cs,ast) =
        annotateComments (fromMaybe m $ applyFixities baseFixities m) comments
      csComments = map comInfoComment cs
  in Right (runPrinterStyle
              mode'
              style
              -- For the time being, assume that all "free-floating" comments come at the beginning.
              -- If they were not at the beginning, they would be after some ast node.
              -- Thus, print them before going for the ast.
              (do mapM_ (printComment Nothing)
                        (styleCommentPreprocessor style (styleDefConfig style) (reverse csComments))
                  pretty ast))

-- | Pretty print the given printable thing.
runPrinterStyle :: ParseMode -> Style -> (forall s. Printer s ()) -> Builder
runPrinterStyle mode' (Style _name _author _desc st extenders config preprocessor) m =
  maybe (error "Printer failed with mzero call.")
        psOutput
        (runIdentity
           (runMaybeT (execStateT
                         (runPrinter m)
                         (PrintState 0 mempty False 0 1 st extenders config False False mode' preprocessor))))

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
