module CabalFile
  ( getCabalExtensionsForSourcePath
  ) where

import Data.List
import Data.Maybe
import Data.Traversable
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Language.Haskell.Extension
import qualified Language.Haskell.Exts.Extension as HSE
import System.Directory
import System.FilePath
import Text.Read

isUnder :: FilePath -> FilePath -> Bool
isUnder parent child = makeRelative parent child /= child

matchBuildInfo :: FilePath -> BuildInfo -> Bool
matchBuildInfo relsrcpath bi =
  any (\parent -> isUnder parent relsrcpath) $ hsSourceDirs bi

findCabalFiles :: FilePath -> FilePath -> IO (Maybe ([FilePath], FilePath))
findCabalFiles dir rel = do
  names <- getDirectoryContents dir
  let cabalnames = filter (isSuffixOf ".cabal") names
  case cabalnames of
    []
      | dir == "/" -> return Nothing
    [] -> findCabalFiles (takeDirectory dir) (takeFileName dir </> rel)
    _ -> return $ Just (fmap (\n -> dir </> n) cabalnames, rel)

allBuildInfo' :: PackageDescription -> [BuildInfo]
allBuildInfo' pd =
  mconcat
    [ maybeToList $ fmap libBuildInfo $ library pd
    , fmap buildInfo $ executables pd
    , fmap testBuildInfo $ testSuites pd
    , fmap benchmarkBuildInfo $ benchmarks pd
    ]

getBest :: Ord i => [(a, i)] -> Maybe (a, i)
getBest [] = Nothing
getBest ((a, i):rr) =
  Just $
  case getBest rr of
    Nothing -> (a, i)
    Just (a', i') ->
      if i' > i
        then (a', i')
        else (a, i)

getCabalStanza :: FilePath -> IO (Maybe BuildInfo)
getCabalStanza srcpath = do
  abssrcpath <- canonicalizePath srcpath
  mcp <- findCabalFiles (takeDirectory abssrcpath) (takeFileName abssrcpath)
  case mcp of
    Just (cabalpaths, relpath) -> do
      biss <-
        for cabalpaths $ \cabalpath -> do
          cabaltext <- readFile cabalpath
          case parsePackageDescription cabaltext of
            ParseFailed _ -> return []
            ParseOk _ gpd -> do
              return $ allBuildInfo' $ flattenPackageDescription gpd
      let bis = filter (matchBuildInfo relpath) $ mconcat biss
      return $
        fmap fst $
        getBest $ do
          bi <- bis
          dir <- hsSourceDirs bi
          -- the best one is the one with the longest hsSourceDirs
          return (bi, length dir)
    Nothing -> return Nothing

getCabalExtensions :: FilePath -> IO (Language, [Extension])
getCabalExtensions srcpath = do
  mbi <- getCabalStanza srcpath
  return $
    case mbi of
      Nothing -> (Haskell98, [])
      Just bi -> do
        (fromMaybe Haskell98 $ defaultLanguage bi, defaultExtensions bi)

convertLanguage :: Language -> HSE.Language
convertLanguage lang = read $ show lang

convertKnownExtension :: KnownExtension -> Maybe HSE.KnownExtension
convertKnownExtension ext =
  case readEither $ show ext of
    Left _ -> Nothing
    Right hext -> Just hext

convertExtension :: Extension -> Maybe HSE.Extension
convertExtension (EnableExtension ke) =
  fmap HSE.EnableExtension $ convertKnownExtension ke
convertExtension (DisableExtension ke) =
  fmap HSE.DisableExtension $ convertKnownExtension ke
convertExtension (UnknownExtension s) = Just $ HSE.UnknownExtension s

getCabalExtensionsForSourcePath :: FilePath -> IO [HSE.Extension]
getCabalExtensionsForSourcePath srcpath = do
  (lang, exts) <- getCabalExtensions srcpath
  return $
    fmap HSE.EnableExtension $
    HSE.toExtensionList (convertLanguage lang) $ mapMaybe convertExtension exts
