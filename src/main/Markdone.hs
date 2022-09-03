{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | A subset of markdown that only supports @#headings@ and code
-- fences.
--
-- All content must be in section headings with proper hierarchy,
-- anything else is rejected.

module Markdone where

import           Control.DeepSeq
import           Control.Monad.Catch
import           Control.Monad.State.Strict (State, evalState, get, put)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Char
import           Data.Typeable
import           GHC.Generics

-- | A markdone token.
data Token
  = Heading !Int
            !ByteString
  | PlainLine !ByteString
  | BeginFence !ByteString
  | EndFence
  deriving (Eq, Show)

-- | A markdone document.
data Markdone
  = Section !ByteString
            ![Markdone]
  | CodeFence !ByteString
              !ByteString
  | PlainText !ByteString
  deriving (Eq,Show,Generic)
instance NFData Markdone

-- | Parse error.
data MarkdownError = NoFenceEnd | ExpectedSection
  deriving (Typeable,Show)
instance Exception MarkdownError

data TokenizerMode
  = Normal
  | Fenced

-- | Tokenize the bytestring.
tokenize :: ByteString -> [Token]
tokenize input = evalState (mapM token (S8.filter (/= '\r') <$> S8.lines input)) Normal
  where
    token :: ByteString -> State TokenizerMode Token
    token line = do
      mode <- get
      case mode of
        Normal ->
          if S8.isPrefixOf "#" line
            then let (hashes,title) = S8.span (== '#') line
                 in return $
                      Heading (S8.length hashes) (S8.dropWhile isSpace title)
            else if S8.isPrefixOf "```" line
                   then do
                     put Fenced
                     return $
                       BeginFence
                         (S8.dropWhile
                            (\c ->
                                c == '`' || c == ' ')
                            line)
                   else return $ PlainLine line
        Fenced ->
          if line == "```"
            then do
              put Normal
              return EndFence
            else return $ PlainLine line

-- | Parse into a forest.
parse :: (Functor m,MonadThrow m) => [Token] -> m [Markdone]
parse = go (0 :: Int)
  where
    go level =
      \case
        (Heading n label:rest) ->
          let (children,rest') =
                span
                  (\case
                     Heading nextN _ -> nextN > n
                     _ -> True)
                  rest
          in do childs <- go (level + 1) children
                siblings <- go level rest'
                return (Section label childs : siblings)
        (BeginFence label:rest)
          | level > 0 ->
            let (content,rest') =
                  (span
                     (\case
                        PlainLine {} -> True
                        _ -> False)
                     rest)
            in case rest' of
                 (EndFence:rest'') ->
                   fmap
                     (CodeFence
                        label
                        (S8.intercalate "\n" (map getPlain content)) :)
                     (go level rest'')
                 _ -> throwM NoFenceEnd
        PlainLine p:rest
          | level > 0 ->
            let (content,rest') =
                  (span
                     (\case
                        PlainLine {} -> True
                        _ -> False)
                     (PlainLine p : rest))
            in fmap
                 (PlainText
                    (S8.intercalate
                       "\n"
                       (filter (not . S8.null) (map getPlain content))) :)
                 (go level rest')
        [] -> return []
        _ -> throwM ExpectedSection
    getPlain (PlainLine x) = x
    getPlain _ = ""
