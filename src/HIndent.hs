{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings, TupleSections, ScopedTypeVariables, PatternGuards #-}

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
import           Data.Either
import           Data.Function
import           Data.Functor.Identity
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable hiding (mapM)
import           HIndent.Pretty
import           HIndent.Types
import qualified Language.Haskell.Exts as Exts
import           Language.Haskell.Exts hiding (Style, prettyPrint, Pretty, style, parse)
import           Prelude

-- | A block of code.
data CodeBlock
    = Shebang ByteString
    | HaskellSource Int ByteString
    -- ^ Includes the starting line (indexed from 0) for error reporting
    | CPPDirectives ByteString
     deriving (Show, Eq)

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
                         mode' { extensions = exts' ++ extensions mode' }
                       Just (Just lang, exts') ->
                         mode' { baseLanguage = lang
                               , extensions = exts' ++ extensions mode' }
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
cppSplitBlocks :: ByteString -> [CodeBlock]
cppSplitBlocks inp =
  modifyLast (inBlock (<> trailing)) .
  map (classify . unlines') .
  groupBy ((==) `on` nonHaskellLine) . zip [0 ..] . S8.lines $
  inp
  where
    nonHaskellLine :: (Int, ByteString) -> Bool
    nonHaskellLine (_, src) = cppLine src || shebangLine src
    shebangLine :: ByteString -> Bool
    shebangLine = S8.isPrefixOf "#!"
    cppLine :: ByteString -> Bool
    cppLine src =
      any
        (`S8.isPrefixOf` src)
        ["#if", "#end", "#else", "#define", "#undef", "#elif", "#include", "#error", "#warning"]
        -- Note: #ifdef and #ifndef are handled by #if
    unlines' :: [(Int, ByteString)] -> (Int, ByteString)
    unlines' [] = (0, S.empty)
    unlines' srcs@((line, _):_) =
      (line, mconcat . intersperse "\n" $ map snd srcs)
    classify :: (Int, ByteString) -> CodeBlock
    classify (line, text)
      | shebangLine text = Shebang text
      | cppLine text = CPPDirectives text
      | otherwise = HaskellSource line text
    -- Hack to work around some parser issues in haskell-src-exts: Some pragmas
    -- need to have a newline following them in order to parse properly, so we include
    -- the trailing newline in the code block if it existed.
    trailing :: ByteString
    trailing =
      if S8.isSuffixOf "\n" inp
        then "\n"
        else ""
    modifyLast :: (a -> a) -> [a] -> [a]
    modifyLast _ [] = []
    modifyLast f [x] = [f x]
    modifyLast f (x:xs) = x : modifyLast f xs
    inBlock :: (ByteString -> ByteString) -> CodeBlock -> CodeBlock
    inBlock f (HaskellSource line txt) = HaskellSource line (f txt)
    inBlock _ dir = dir


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
              , psHardLimit = False
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

-- | Parse an extension.
readExtension :: String -> Maybe Extension
readExtension x =
  case classifyExtension x -- Foo
       of
    UnknownExtension _ -> Nothing
    x' -> Just x'

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
          (<>)
          CommentAfterLine
          (\nodeSpan commentSpan ->
              fst (srcSpanStart commentSpan) >= fst (srcSpanEnd nodeSpan)))) <=<
  shortCircuit
    (traverse
     -- Collect forwards comments which start at the end line of a
     -- node: Does the start line of the comment match the end-line
     -- of the node?
       (collectCommentsBy
          (<>)
          CommentSameLine
          (\nodeSpan commentSpan ->
              fst (srcSpanStart commentSpan) == fst (srcSpanEnd nodeSpan)))) <=<
  shortCircuit
    (traverseBackwards
     -- Collect backwards comments which are on the same line as a
     -- node: Does the start line & end line of the comment match
     -- that of the node?
       (collectCommentsBy
          (<>)
          CommentSameLine
          (\nodeSpan commentSpan ->
              fst (srcSpanStart commentSpan) == fst (srcSpanStart nodeSpan) &&
              fst (srcSpanStart commentSpan) == fst (srcSpanEnd nodeSpan)))) <=<
  shortCircuit
    (traverse
     -- First, collect forwards comments for declarations which both
     -- start on column 1 and occur before the declaration.
       (collectCommentsBy
          (<>)
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
  :: ([NodeComment] -> [NodeComment] -> [NodeComment])
  -> (SrcSpan -> SomeComment -> NodeComment)
  -> (SrcSpan -> SrcSpan -> Bool)
  -> NodeInfo
  -> State [Comment] NodeInfo
collectCommentsBy append cons predicate nodeInfo@(NodeInfo (SrcSpanInfo nodeSpan _) _) = do
  comments <- get
  let (others, mine) =
        partitionEithers
          (map
             (\comment@(Comment multiLine commentSpan commentString) ->
                 if predicate nodeSpan (setFilename commentString commentSpan)
                   then Right
                          (cons
                             commentSpan
                             ((if multiLine
                                 then MultiLine
                                 else EndOfLine)
                                commentString))
                   else Left comment)
             comments)
  put others
  return
    (nodeInfo
     { nodeInfoComments = append (nodeInfoComments nodeInfo) mine
     })
  where
    setFilename cs sp =
      sp
      { srcSpanFilename = cs
      }
