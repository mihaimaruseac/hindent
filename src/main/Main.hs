{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Main entry point to hindent.
--
-- hindent

module Main where

import           HIndent
import           HIndent.Types

import           Control.Applicative
import           Control.Applicative.QQ.Idiom
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.IO as T
import           Data.Version (showVersion)
import           Descriptive
import           Descriptive.Options
import           GHC.Tuple
import           Language.Haskell.Exts.Annotated hiding (Style,style)
import           Paths_hindent (version)
import           System.Environment
import           Text.Read

-- | Main entry point.
main :: IO ()
main =
  do args <- getArgs
     case consume options (map T.pack args) of
       Succeeded (style,exts) ->
         T.interact
           (either error T.toLazyText .
            reformat style (Just exts))
       Failed (Wrap (Stopped Version) _) ->
         putStrLn ("hindent " ++ showVersion version)
       _ ->
         error (T.unpack (textDescription (describe options [])))

-- | Options that stop the argument parser.
data Stoppers = Version
  deriving (Show)

-- | Program options.
options :: Monad m
        => Consumer [Text] (Option Stoppers) m (Style,[Extension])
options =
  ver *>
  [i|(,) style exts|]
  where ver =
          stop (flag "version" "Print the version" Version)
        style =
          [i|makeStyle (constant "--style" "Style to print with" () *>
                        foldr1 (<|>)
                               (map (\s ->
                                       constant (styleName s)
                                                (styleDescription s)
                                                s)
                                    styles))
                       lineLen|]
        exts =
          fmap getExtensions (many (prefix "X" "Language extension"))
        lineLen =
          fmap (>>= (readMaybe . T.unpack))
               (optional (arg "line-length" "Desired length of lines"))
        makeStyle s mlen =
          case mlen of
            Nothing -> s
            Just len ->
              s {styleDefConfig =
                   (styleDefConfig s) {configMaxColumns = len}}

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
  case classifyExtension x of
    UnknownExtension _ -> Nothing
    x' -> Just x'

-- | Default extensions.
defaultExtensions :: [Extension]
defaultExtensions =
  [e | e@EnableExtension{} <- knownExtensions] \\
  map EnableExtension badExtensions

-- | Extensions which steal too much syntax.
badExtensions :: [KnownExtension]
badExtensions =
    [Arrows -- steals proc
    ,TransformListComp -- steals the group keyword
    ,XmlSyntax, RegularPatterns -- steals a-b
    ,UnboxedTuples -- breaks (#) lens operator
    -- ,QuasiQuotes -- breaks [x| ...], making whitespace free list comps break
    ]
