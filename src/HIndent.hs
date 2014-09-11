{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Haskell indenter.

module HIndent
  (-- * Formatting functions.
   reformat
  ,prettyPrint
  ,parseMode
  -- * Style
  ,styles
  ,chrisDone
  ,johanTibell
  ,fundamental
  -- * Testing
  ,test
  ,testAll)
  where

import           Data.Function
import           HIndent.Pretty
import           HIndent.Styles.ChrisDone
import           HIndent.Styles.Fundamental
import           HIndent.Styles.JohanTibell
import           HIndent.Types

import           Control.Monad.State
import           Data.Data
import           Data.Monoid
import qualified Data.Text.IO as ST
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.IO as T
import           Data.Traversable
import           Language.Haskell.Exts.Annotated hiding (Style,prettyPrint,Pretty,style,parse)

-- | Format the given source.
reformat :: Config -> Style -> Text -> Either String Builder
reformat config style x =
  case parseDeclWithComments parseMode
                             (T.unpack x) of
    ParseOk (v,comments) ->
      case annotateComments v comments of
        (cs,ast) ->
          Right (prettyPrint
                   config
                   style
                   (do mapM_ printComment cs
                       pretty ast))
    ParseFailed _ e -> Left e

-- | Pretty print the given printable thing.
prettyPrint :: Config -> Style -> Printer () -> Builder
prettyPrint config style m =
  psOutput (execState (runPrinter m)
                      (case style of
                         Style _name _author _desc st extenders _defconfig ->
                           PrintState 0 mempty False 0 1 st extenders config False))

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
test :: Config -> Style -> Text -> IO ()
test config style =
  either error (T.putStrLn . T.toLazyText) .
  reformat config style

-- | Test with the given style, prints to stdout.
testAll :: Config -> Text -> IO ()
testAll config i =
  forM_ styles
        (\style ->
           do ST.putStrLn ("-- " <> styleName style <> ":")
              test config style i
              ST.putStrLn "")

-- | Styles list, useful for programmatically choosing.
styles :: [Style]
styles =
  [fundamental,chrisDone,johanTibell]

-- | Annotate the AST with comments.
annotateComments :: (Data (ast NodeInfo),Traversable ast,Annotated ast)
                 => ast SrcSpanInfo -> [Comment] -> ([ComInfo],ast NodeInfo)
annotateComments =
  foldr (\c@(Comment _ cspan _) (cs,ast) ->
           case execState (traverse (collect c) ast) Nothing of
             Nothing ->
               (ComInfo c True :
                cs
               ,ast)
             Just l ->
               let ownLine =
                     srcSpanStartLine cspan /=
                     srcSpanEndLine (srcInfoSpan l)
               in (cs
                  ,evalState (traverse (insert l (ComInfo c ownLine)) ast) False)) .
  ([],) .
  fmap (\n -> NodeInfo n [])
  where collect c ni@(NodeInfo newL _) =
          do when (commentAfter ni c)
                  (modify (\ml ->
                             maybe (Just newL)
                                   (\oldL ->
                                      Just (if on spanBefore srcInfoSpan oldL newL
                                               then newL
                                               else oldL))
                                   ml))
             return ni
        insert al c ni@(NodeInfo bl cs) =
          do done <- get
             if not done && al == bl
                then do put True
                        return (ni {nodeInfoComments = c : cs})
                else return ni

-- | Is the comment after the node?
commentAfter :: NodeInfo -> Comment -> Bool
commentAfter (NodeInfo (SrcSpanInfo n _) _) (Comment _ c _) =
  spanBefore n c

-- | Does the first span end before the second starts?
spanBefore :: SrcSpan -> SrcSpan -> Bool
spanBefore before after =
  (srcSpanStartLine after > srcSpanEndLine before) ||
  ((srcSpanStartLine after == srcSpanEndLine before) &&
   (srcSpanStartColumn after > srcSpanEndColumn before))
