{-# LANGUAGE OverloadedStrings #-}

-- | Things related to HIndent configuration.
module HIndent.Config
  ( Config(..)
  , defaultConfig
  , getConfig
  ) where

import Control.Applicative
import Data.Int
import Data.Maybe
import Data.Yaml
import qualified Data.Yaml as Y
import HIndent.LanguageExtension.Conversion
import HIndent.LanguageExtension.Types
import qualified HIndent.Path.Find as Path
import Path
import qualified Path.IO as Path

-- | Configurations shared among the different styles. Styles may pay
-- attention to or completely disregard this configuration.
data Config =
  Config
    { configMaxColumns :: !Int64 -- ^ Maximum columns to fit code into ideally.
    , configIndentSpaces :: !Int64 -- ^ How many spaces to indent?
    , configTrailingNewline :: !Bool -- ^ End with a newline.
    , configSortImports :: !Bool -- ^ Sort imports in groups.
    , configLineBreaks :: [String] -- ^ Break line when meets these operators.
    , configExtensions :: [Extension]
      -- ^ Extra language extensions enabled by default.
    }

instance FromJSON Config where
  parseJSON (Y.Object v) =
    Config <$>
    fmap (fromMaybe (configMaxColumns defaultConfig)) (v Y..:? "line-length") <*>
    fmap
      (fromMaybe (configIndentSpaces defaultConfig))
      (v Y..:? "indent-size" <|> v Y..:? "tab-size") <*>
    fmap
      (fromMaybe (configTrailingNewline defaultConfig))
      (v Y..:? "force-trailing-newline") <*>
    fmap (fromMaybe (configSortImports defaultConfig)) (v Y..:? "sort-imports") <*>
    fmap (fromMaybe (configLineBreaks defaultConfig)) (v Y..:? "line-breaks") <*>
    (traverse convertExt . fromMaybe [] =<< v Y..:? "extensions")
    where
      convertExt x =
        case strToExt x of
          Just x' -> pure x'
          Nothing -> error $ "Unknow extension: " ++ show x
  parseJSON _ = fail "Expected Object for Config value"

-- | Default style configuration.
defaultConfig :: Config
defaultConfig =
  Config
    { configMaxColumns = 80
    , configIndentSpaces = 2
    , configTrailingNewline = True
    , configSortImports = True
    , configLineBreaks = []
    , configExtensions = []
    }

-- | Read config from a config file, or return 'defaultConfig'.
getConfig :: IO Config
getConfig = do
  cur <- Path.getCurrentDir
  homeDir <- Path.getHomeDir
  mfile <-
    Path.findFileUp
      cur
      ((== ".hindent.yaml") . toFilePath . filename)
      (Just homeDir)
  case mfile of
    Nothing -> return defaultConfig
    Just file -> do
      result <- Y.decodeFileEither (toFilePath file)
      case result of
        Left e -> error (show e)
        Right config -> return config
