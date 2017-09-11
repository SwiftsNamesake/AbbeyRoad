
module Main (main) where

import qualified Distribution.PackageDescription as PD
import Distribution.Simple
  ( Args
  , UserHooks
  , buildHook
  , confHook
  , defaultMainWithHooks
  , postClean
  , postConf
  , preConf
  , simpleUserHooks
  )
import Distribution.Simple.LocalBuildInfo
  ( LocalBuildInfo
  , configFlags
  , localPkgDescr
  )
import Distribution.Simple.Setup
  ( BuildFlags
  , CleanFlags
  , ConfigFlags
  , buildVerbosity
  , cleanVerbosity
  , configConfigurationsFlags
  , configVerbosity
  , fromFlag
  )
import System.Directory (getCurrentDirectory)
import System.FilePath  ((</>))
import Data.Maybe (fromJust)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { confHook = configHook }


-- |
-- TODO | - All this, just to update two fields (?)
--        - Use `preConf` or `confHook` (?)
configHook :: (PD.GenericPackageDescription, PD.HookedBuildInfo) ->
               ConfigFlags ->
               IO LocalBuildInfo
configHook (description, buildInfo) flags = do
  putStrLn "\n\n\nRunning config hook, appending paths\n\n\n"
  localBuildInfo <- confHook simpleUserHooks (description, buildInfo) flags
  dir <- getCurrentDirectory -- TODO: Make sure this is correct
  let packageDescription = localPkgDescr localBuildInfo
      library = fromJust $ PD.library packageDescription
      libraryBuildInfo = PD.libBuildInfo library

      alutLibPath    = dir </> "dependencies/freealut/admin/VisualStudio6/alut/x64/Release"
      alutHeaderPath = dir </> "dependencies/freealut/include"

      alLibPath    = dir </> "dependencies/openal-soft-1.16.0-bin/openal-soft-1.16.0-bin/libs/Win64"
      alHeaderPath = dir </> "dependencies/openal-soft-1.16.0-bin/openal-soft-1.16.0-bin/include"
  return localBuildInfo {
    localPkgDescr = packageDescription {
      PD.library = Just $ library {
        PD.libBuildInfo = libraryBuildInfo {
            PD.includeDirs  = alutHeaderPath : alHeaderPath : PD.includeDirs libraryBuildInfo,
            PD.extraLibDirs = alutLibPath    : alLibPath    : PD.extraLibDirs libraryBuildInfo
        }
      }
    }
  }
