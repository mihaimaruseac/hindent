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

import           HIndent.Pretty
import           HIndent.Styles.ChrisDone (chrisDone)
import           HIndent.Styles.Fundamental (fundamental)
import           HIndent.Styles.Gibiansky (gibiansky)
import           HIndent.Styles.JohanTibell (johanTibell)
import           HIndent.Types

import           Control.Monad.State.Strict
import           Data.Data
import           Data.Function
import           Data.Monoid
import qualified Data.Text.IO as ST
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.IO as T
import           Data.Traversable
import           Language.Haskell.Exts.Annotated hiding (Style,prettyPrint,Pretty,style,parse)
import           Data.Maybe (fromMaybe)

-- | Format the given source.
reformat :: Style -> Text -> Either String Builder
reformat style x =
  case parseModuleWithComments parseMode
                               (T.unpack x) of
    ParseOk (mod,comments) ->
      let (cs,ast) =
            annotateComments (fromMaybe mod $ applyFixities baseFixities mod) comments
      in Right (prettyPrint
                  style
                  -- For the time being, assume that all "free-floating" comments come at the beginning.
                  -- If they were not at the beginning, they would be after some ast node.
                  -- Thus, print them before going for the ast.
                  (do mapM_ (printComment Nothing) cs
                      pretty ast))
    ParseFailed _ e -> Left e

-- | Pretty print the given printable thing.
prettyPrint :: Style -> Printer () -> Builder
prettyPrint style m =
  psOutput (execState (runPrinter m)
                      (case style of
                         Style _name _author _desc st extenders config ->
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
test :: Style -> Text -> IO ()
test style =
  either error (T.putStrLn . T.toLazyText) .
  reformat style

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
    ParseOk (mod,comments) ->
      Right (annotateComments mod comments)
    ParseFailed _ e -> Left e

-- | Styles list, useful for programmatically choosing.
styles :: [Style]
styles =
  [fundamental,chrisDone,johanTibell,gibiansky]

-- | Annotate the AST with comments.
annotateComments :: forall ast. (Data (ast NodeInfo),Traversable ast,Annotated ast)
                 => ast SrcSpanInfo -> [Comment] -> ([ComInfo],ast NodeInfo)
annotateComments =
  -- Add all comments to the ast.
  foldr processComment .
  -- Turn result into a tuple, with ast as second element.
  ([],) .
  -- Replace source spans with node infos in the AST.
  -- The node infos have empty comment lists.
  fmap (\n -> NodeInfo n [])
  where processComment :: Comment
                       -> ([ComInfo],ast NodeInfo)
                       -> ([ComInfo],ast NodeInfo)
        -- Add in a single comment to the ast.
        processComment c@(Comment _ cspan _) (cs,ast) =
          -- Try to find the node after which this comment lies.
          case execState (traverse (collect c) ast) Nothing of
            -- When no node is found, the comment is on its own line.
            Nothing ->
              (ComInfo c True :
               cs
              ,ast)
            -- We found the node that this comment follows.
            -- Insert this comment into the ast.
            Just l ->
              let ownLine =
                    srcSpanStartLine cspan /=
                    srcSpanEndLine (srcInfoSpan l)
              in (cs
                 ,evalState (traverse (insert l (ComInfo c ownLine)) ast) False)
        -- For a comment, check whether the comment is after the node.
        -- If it is, store it in the state; otherwise do nothing.
        collect :: Comment -> NodeInfo -> State (Maybe SrcSpanInfo) NodeInfo
        collect c ni@(NodeInfo newL _) =
          do when (commentAfter ni c)
                  (modify (maybe (Just newL)
                                 (\oldL ->
                                    Just (if (spanBefore `on` srcInfoSpan) oldL newL
                                             then newL
                                             else oldL))))
             return ni
        -- Insert the comment into the ast. Find the right node and add it to the
        -- comments of that node. Do nothing afterwards.
        insert :: SrcSpanInfo -> ComInfo -> NodeInfo -> State Bool NodeInfo
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
