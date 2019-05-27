{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, PatternGuards #-}

-- | Haskell indenter.

module HIndent
  (-- * Formatting functions.
   reformat
  ,prettyPrint
  ,parseMode
  -- * Testing
  ,test
  ,testFile
  ,testAst
  ,testFileAst
  ,defaultExtensions
  ,getExtensions
  )
  where

import           Control.Monad.State.Strict
import           Control.Monad.Trans.Maybe
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Unsafe as S
import           Data.Char
import           Data.Foldable (foldr')
import           Data.Either
import           Data.Function
import           Data.Functor.Identity
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable hiding (mapM)
import           HIndent.CodeBlock
import           HIndent.Pretty
import           HIndent.Types
import qualified Language.Haskell.Exts as Exts
import           Language.Haskell.Exts hiding (Style, prettyPrint, Pretty, style, parse)
import           Prelude

-- | Format the given source.
reformat :: Config -> Maybe [Extension] -> Maybe FilePath -> ByteString -> Either String Builder
reformat config mexts mfilepath =
    preserveTrailingNewline
        (fmap (mconcat . intersperse "\n") . mapM processBlock . cppSplitBlocks)
  where
    processBlock :: CodeBlock -> Either String Builder
    processBlock (Shebang text) = Right $ S.byteString text
    processBlock (CPPDirectives text) = Right $ S.byteString text
    processBlock (HaskellSource line text) =
        let ls = S8.lines text
            prefix = findPrefix ls
            code = unlines' (map (stripPrefix prefix) ls)
            exts = readExtensions (UTF8.toString code)
            mode'' = case exts of
                       Nothing -> mode'
                       Just (Nothing, exts') ->
                         mode' { extensions =
                                   exts'
                                   ++ configExtensions config
                                   ++ extensions mode' }
                       Just (Just lang, exts') ->
                         mode' { baseLanguage = lang
                               , extensions =
                                   exts'
                                   ++ configExtensions config
                                   ++ extensions mode' }
        in case parseModuleWithComments mode'' (UTF8.toString code) of
               ParseOk (m, comments) ->
                   fmap
                       (S.lazyByteString . addPrefix prefix . S.toLazyByteString)
                       (prettyPrint config m comments)
               ParseFailed loc e ->
                 Left (Exts.prettyPrint (loc {srcLine = srcLine loc + line}) ++ ": " ++ e)
    unlines' = S.concat . intersperse "\n"
    unlines'' = L.concat . intersperse "\n"
    addPrefix :: ByteString -> L8.ByteString -> L8.ByteString
    addPrefix prefix = unlines'' . map (L8.fromStrict prefix <>) . L8.lines
    stripPrefix :: ByteString -> ByteString -> ByteString
    stripPrefix prefix line =
        if S.null (S8.dropWhile (== '\n') line)
            then line
            else fromMaybe (error "Missing expected prefix") . s8_stripPrefix prefix $
                 line
    findPrefix :: [ByteString] -> ByteString
    findPrefix = takePrefix False . findSmallestPrefix . dropNewlines
    dropNewlines :: [ByteString] -> [ByteString]
    dropNewlines = filter (not . S.null . S8.dropWhile (== '\n'))
    takePrefix :: Bool -> ByteString -> ByteString
    takePrefix bracketUsed txt =
        case S8.uncons txt of
            Nothing -> ""
            Just ('>', txt') ->
                if not bracketUsed
                    then S8.cons '>' (takePrefix True txt')
                    else ""
            Just (c, txt') ->
                if c == ' ' || c == '\t'
                    then S8.cons c (takePrefix bracketUsed txt')
                    else ""
    findSmallestPrefix :: [ByteString] -> ByteString
    findSmallestPrefix [] = ""
    findSmallestPrefix ("":_) = ""
    findSmallestPrefix (p:ps) =
        let first = S8.head p
            startsWithChar c x = S8.length x > 0 && S8.head x == c
        in if all (startsWithChar first) ps
               then S8.cons
                        first
                        (findSmallestPrefix (S.tail p : map S.tail ps))
               else ""
    mode' =
        let m = case mexts of
                  Just exts ->
                    parseMode
                    { extensions = exts
                    }
                  Nothing -> parseMode
        in m { parseFilename = fromMaybe "<interactive>" mfilepath }
    preserveTrailingNewline f x =
        if S8.null x || S8.all isSpace x
            then return mempty
            else if hasTrailingLine x || configTrailingNewline config
                     then fmap
                              (\x' ->
                                    if hasTrailingLine
                                           (L.toStrict (S.toLazyByteString x'))
                                        then x'
                                        else x' <> "\n")
                              (f x)
                     else f x

-- | Does the strict bytestring have a trailing newline?
hasTrailingLine :: ByteString -> Bool
hasTrailingLine xs =
    if S8.null xs
        then False
        else S8.last xs == '\n'

-- | Print the module.
prettyPrint :: Config
            -> Module SrcSpanInfo
            -> [Comment]
            -> Either a Builder
prettyPrint config m comments =
  let ast =
        evalState
          (collectAllComments
             (fromMaybe m (applyFixities baseFixities m)))
          comments
  in Right (runPrinterStyle config (pretty ast))

-- | Pretty print the given printable thing.
runPrinterStyle :: Config -> Printer () -> Builder
runPrinterStyle config m =
  maybe
    (error "Printer failed with mzero call.")
    psOutput
    (runIdentity
       (runMaybeT
          (execStateT
             (runPrinter m)
             (PrintState
              { psIndentLevel = 0
              , psOutput = mempty
              , psNewline = False
              , psColumn = 0
              , psLine = 1
              , psConfig = config
              , psInsideCase = False
              , psFitOnOneLine = False
              , psEolComment = False
              }))))

-- | Parse mode, includes all extensions, doesn't assume any fixities.
parseMode :: ParseMode
parseMode =
  defaultParseMode {extensions = allExtensions
                   ,fixities = Nothing}
  where allExtensions =
          filter isDisabledExtension knownExtensions
        isDisabledExtension (DisableExtension _) = False
        isDisabledExtension _ = True

-- | Test the given file.
testFile :: FilePath -> IO ()
testFile fp  = S.readFile fp >>= test

-- | Test the given file.
testFileAst :: FilePath -> IO ()
testFileAst fp  = S.readFile fp >>= print . testAst

-- | Test with the given style, prints to stdout.
test :: ByteString -> IO ()
test =
  either error (L8.putStrLn . S.toLazyByteString) .
  reformat defaultConfig Nothing Nothing

-- | Parse the source and annotate it with comments, yielding the resulting AST.
testAst :: ByteString -> Either String (Module NodeInfo)
testAst x =
  case parseModuleWithComments parseMode (UTF8.toString x) of
    ParseOk (m,comments) ->
      Right
        (let ast =
               evalState
                 (collectAllComments
                    (fromMaybe m (applyFixities baseFixities m)))
                 comments
         in ast)
    ParseFailed _ e -> Left e

-- | Default extensions.
defaultExtensions :: [Extension]
defaultExtensions =
  [ e
  | e@EnableExtension {} <- knownExtensions ] \\
  map EnableExtension badExtensions

-- | Extensions which steal too much syntax.
badExtensions :: [KnownExtension]
badExtensions =
    [Arrows -- steals proc
    ,TransformListComp -- steals the group keyword
    ,XmlSyntax, RegularPatterns -- steals a-b
    ,UnboxedTuples -- breaks (#) lens operator
    -- ,QuasiQuotes -- breaks [x| ...], making whitespace free list comps break
    ,PatternSynonyms -- steals the pattern keyword
    ,RecursiveDo -- steals the rec keyword
    ,DoRec -- same
    ,TypeApplications -- since GHC 8 and haskell-src-exts-1.19
    ]


s8_stripPrefix :: ByteString -> ByteString -> Maybe ByteString
s8_stripPrefix bs1@(S.PS _ _ l1) bs2
   | bs1 `S.isPrefixOf` bs2 = Just (S.unsafeDrop l1 bs2)
   | otherwise = Nothing

--------------------------------------------------------------------------------
-- Extensions stuff stolen from hlint

-- | Consume an extensions list from arguments.
getExtensions :: [Text] -> [Extension]
getExtensions = foldl f defaultExtensions . map T.unpack
  where f _ "Haskell98" = []
        f a ('N':'o':x)
          | Just x' <- readExtension x =
            delete x' a
        f a x
          | Just x' <- readExtension x =
            x' :
            delete x' a
        f _ x = error $ "Unknown extension: " ++ x

--------------------------------------------------------------------------------
-- Comments

-- | Traverse the structure backwards.
traverseInOrder
  :: (Monad m, Traversable t, Functor m)
  => (b -> b -> Ordering) -> (b -> m b) -> t b -> m (t b)
traverseInOrder cmp f ast = do
  indexed <-
    fmap (zip [0 :: Integer ..] . reverse) (execStateT (traverse (modify . (:)) ast) [])
  let sorted = sortBy (\(_,x) (_,y) -> cmp x y) indexed
  results <-
    mapM
      (\(i,m) -> do
         v <- f m
         return (i, v))
      sorted
  evalStateT
    (traverse
       (const
          (do i <- gets head
              modify tail
              case lookup i results of
                Nothing -> error "traverseInOrder"
                Just x -> return x))
       ast)
    [0 ..]

-- | Collect all comments in the module by traversing the tree. Read
-- this from bottom to top.
collectAllComments :: Module SrcSpanInfo -> State [Comment] (Module NodeInfo)
collectAllComments =
  shortCircuit
    (traverseBackwards
     -- Finally, collect backwards comments which come after each node.
       (collectCommentsBy
          CommentAfterLine
          (\nodeSpan commentSpan ->
              fst (srcSpanStart commentSpan) >= fst (srcSpanEnd nodeSpan)))) <=<
  shortCircuit addCommentsToTopLevelWhereClauses <=<
  shortCircuit
    (traverse
     -- Collect forwards comments which start at the end line of a
     -- node: Does the start line of the comment match the end-line
     -- of the node?
       (collectCommentsBy
          CommentSameLine
          (\nodeSpan commentSpan ->
              fst (srcSpanStart commentSpan) == fst (srcSpanEnd nodeSpan)))) <=<
  shortCircuit
    (traverseBackwards
     -- Collect backwards comments which are on the same line as a
     -- node: Does the start line & end line of the comment match
     -- that of the node?
       (collectCommentsBy
          CommentSameLine
          (\nodeSpan commentSpan ->
              fst (srcSpanStart commentSpan) == fst (srcSpanStart nodeSpan) &&
              fst (srcSpanStart commentSpan) == fst (srcSpanEnd nodeSpan)))) <=<
  shortCircuit
    (traverse
     -- First, collect forwards comments for declarations which both
     -- start on column 1 and occur before the declaration.
       (collectCommentsBy
          CommentBeforeLine
          (\nodeSpan commentSpan ->
              (snd (srcSpanStart nodeSpan) == 1 &&
               snd (srcSpanStart commentSpan) == 1) &&
              fst (srcSpanStart commentSpan) < fst (srcSpanStart nodeSpan)))) .
  fmap nodify
  where
    nodify s = NodeInfo s mempty
    -- Sort the comments by their end position.
    traverseBackwards =
      traverseInOrder
        (\x y -> on (flip compare) (srcSpanEnd . srcInfoSpan . nodeInfoSpan) x y)
    -- Stop traversing if all comments have been consumed.
    shortCircuit m v = do
      comments <- get
      if null comments
        then return v
        else m v

-- | Collect comments by satisfying the given predicate, to collect a
-- comment means to remove it from the pool of available comments in
-- the State. This allows for a multiple pass approach.
collectCommentsBy
  :: (SrcSpan -> SomeComment -> NodeComment)
  -> (SrcSpan -> SrcSpan -> Bool)
  -> NodeInfo
  -> State [Comment] NodeInfo
collectCommentsBy cons predicate nodeInfo@(NodeInfo (SrcSpanInfo nodeSpan _) _) = do
  comments <- get
  let (others, mine) =
        partitionEithers
          (map
             (\comment@(Comment _ commentSpan _) ->
                 if predicate nodeSpan commentSpan
                   then Right comment
                   else Left comment)
             comments)
  put others
  return $ addCommentsToNode cons mine nodeInfo

-- | Reintroduce comments which were immediately above declarations in where clauses.
-- Affects where clauses of top level declarations only.
addCommentsToTopLevelWhereClauses ::
     Module NodeInfo -> State [Comment] (Module NodeInfo)
addCommentsToTopLevelWhereClauses (Module x x' x'' x''' topLevelDecls) =
  Module x x' x'' x''' <$>
  traverse addCommentsToWhereClauses topLevelDecls
  where
    addCommentsToWhereClauses ::
         Decl NodeInfo -> State [Comment] (Decl NodeInfo)
    addCommentsToWhereClauses (PatBind x x' x'' (Just (BDecls x''' whereDecls))) = do
      newWhereDecls <- traverse addCommentsToPatBind whereDecls
      return $ PatBind x x' x'' (Just (BDecls x''' newWhereDecls))
    addCommentsToWhereClauses other = return other
    addCommentsToPatBind :: Decl NodeInfo -> State [Comment] (Decl NodeInfo)
    addCommentsToPatBind (PatBind bindInfo (PVar x (Ident declNodeInfo declString)) x' x'') = do
      bindInfoWithComments <- addCommentsBeforeNode bindInfo
      return $
        PatBind
          bindInfoWithComments
          (PVar x (Ident declNodeInfo declString))
          x'
          x''
    addCommentsToPatBind other = return other
    addCommentsBeforeNode :: NodeInfo -> State [Comment] NodeInfo
    addCommentsBeforeNode nodeInfo = do
      comments <- get
      let (notAbove, above) = partitionAboveNotAbove comments nodeInfo
      put notAbove
      return $ addCommentsToNode CommentBeforeLine above nodeInfo
    partitionAboveNotAbove :: [Comment] -> NodeInfo -> ([Comment], [Comment])
    partitionAboveNotAbove cs (NodeInfo (SrcSpanInfo nodeSpan _) _) =
      fst $
      foldr'
        (\comment@(Comment _ commentSpan _) ((ls, rs), lastSpan) ->
           if comment `isAbove` lastSpan
             then ((ls, comment : rs), commentSpan)
             else ((comment : ls, rs), lastSpan))
        (([], []), nodeSpan)
        cs
    isAbove :: Comment -> SrcSpan -> Bool
    isAbove (Comment _ commentSpan _) span =
      let (_, commentColStart) = srcSpanStart commentSpan
          (commentLnEnd, _) = srcSpanEnd commentSpan
          (lnStart, colStart) = srcSpanStart span
       in commentColStart == colStart && commentLnEnd + 1 == lnStart
addCommentsToTopLevelWhereClauses other = return other

addCommentsToNode :: (SrcSpan -> SomeComment -> NodeComment)
                  -> [Comment]
                  -> NodeInfo
                  -> NodeInfo
addCommentsToNode mkNodeComment newComments nodeInfo@(NodeInfo (SrcSpanInfo _ _) existingComments) =
  nodeInfo
    {nodeInfoComments = existingComments <> map mkBeforeNodeComment newComments}
  where
    mkBeforeNodeComment :: Comment -> NodeComment
    mkBeforeNodeComment (Comment multiLine commentSpan commentString) =
      mkNodeComment
        commentSpan
        ((if multiLine
            then MultiLine
            else EndOfLine)
           commentString)
