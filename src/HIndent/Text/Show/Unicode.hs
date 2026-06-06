{-
Copyright Takayuki Muranushi (c) 2016

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Author name here nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
-- | A vendored copy of `Text.Show.Unicode` from `unicode-show` with
-- unused functions trimmed.
module HIndent.Text.Show.Unicode
  ( ushow
  ) where

import Control.Monad.Trans.State.Strict (StateT(StateT, runStateT), get, put)
import Data.Char (isAscii, isPrint)
import qualified Data.List as L
import qualified Data.Ord as O
import Safe (minimumByMay)
import Text.ParserCombinators.ReadP (gather, readP_to_S)
import Text.Read.Lex (lexChar)

-- | Create a parser for less dependencies. ReadP is too slow
type Parser a = StateT String Maybe a

-- | Show the input, and then replace Haskell character literals
-- with the character it represents, for any Unicode printable characters except backslash, single and double quotation marks.
-- If something fails, fallback to standard 'show'.
ushow :: Show a => a -> String
ushow = ushowWith shouldRecover

shouldRecover :: Char -> Bool
shouldRecover c = isPrint c && not (isAscii c)

-- | Show the input, and then replace character literals
-- with the character itself, for characters that satisfy the given predicate.
ushowWith :: Show a => (Char -> Bool) -> a -> String
ushowWith p = urecoverWith p . show

-- | Replace character literals with the character itself, for characters that
-- satisfy the given predicate.
urecoverWith :: (Char -> Bool) -> String -> String
urecoverWith p s =
  case runStateT (recoverChars p) s of
    Just (r, left) -> r ++ left
    Nothing -> s

recoverChars :: (Char -> Bool) -> Parser String
recoverChars p = outsideLiteral
  where
    outsideLiteral = do
      notLit <- untilDoubleQuote
      rest <- get
      case rest of
        '\"':inLiteral -> do
          put inLiteral
          (notLit ++) . ('\"' :) <$> insideLiteral
        _ -> return $ notLit ++ rest
    insideLiteral = do
      recovered <- recoverCharInLiteral
      case recovered of
        ("\"", '"') -> ('"' :) <$> outsideLiteral
        (s, c)
          | p c -> (c :) <$> insideLiteral
          | otherwise -> (s ++) <$> insideLiteral
    untilDoubleQuote = StateT $ Just . L.break (== '\"')

-- | Parse one Haskell character literal expression from a 'String' produced by 'show', and
--   returns the pair of the string before parsed with the parsed character.
--  * Note that special delimiter sequence "\&" may appear in a string. c.f.  <https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-200002.6 Section 2.6 of the Haskell 2010 specification>.
recoverCharInLiteral :: Parser (String, Char)
recoverCharInLiteral =
  StateT $ \s ->
    let result = readP_to_S (gather lexChar) s
          -- The longest match result should leave the shortest string.
          -- So choose the result with the minimum length left.
     in minimumByMay (O.comparing (length . snd)) result
