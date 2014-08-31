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
  ,michaelSnoyman
  ,johanTibell
  ,fundamental
  -- * Testing
  ,test)
  where

import           Data.Function
import           HIndent.Pretty
import           HIndent.Styles.ChrisDone
import           HIndent.Styles.Fundamental
import           HIndent.Styles.JohanTibell
import           HIndent.Styles.MichaelSnoyman
import           HIndent.Types

import           Control.Monad.State
import           Data.Data
import           Data.Monoid
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

-- | Styles list, useful for programmatically choosing.
styles :: [Style]
styles =
  [fundamental,chrisDone,michaelSnoyman,johanTibell]

-- | Annotate the AST with comments.
annotateComments :: (Data (ast NodeInfo),Traversable ast,Annotated ast) => ast SrcSpanInfo -> [Comment] -> ([ComInfo],ast NodeInfo)
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
  where collect c ni@(NodeInfo l _) =
          do when (commentAfter c ni)
                  (modify (\ml ->
                             maybe (Just l)
                                   (\l' ->
                                      Just (if on spanBefore srcInfoSpan l l'
                                               then l'
                                               else l))
                                   ml))
             return ni
        insert al c ni@(NodeInfo bl cs) =
          do done <- get
             if not done && al == bl
                then do put True
                        return (ni {nodeInfoComments = c : cs})
                else return ni

-- | Is the comment after the node?
commentAfter :: Comment -> NodeInfo -> Bool
commentAfter (Comment _ cspan _) (NodeInfo (SrcSpanInfo nspan _) _) =
  spanBefore nspan cspan

-- | Span: a < b
spanBefore :: SrcSpan -> SrcSpan -> Bool
spanBefore a b =
  (srcSpanEndLine a < srcSpanEndLine b) ||
  ((srcSpanEndLine a == srcSpanEndLine b) &&
   (srcSpanEndColumn a < srcSpanEndColumn b))
