{-# LANGUAGE OverloadedStrings #-}

module HSInstall.DeploymentInfo
  ( DeploymentInfo (..)
  , constructDeploymentInfo

  -- re-exporting
  , normal
  )
  where

import Data.List ( isSuffixOf )
import Data.Maybe ( listToMaybe )
import Data.Monoid ( First (..), getFirst )
import Distribution.Package
  ( PackageId
  , PackageIdentifier (pkgName, pkgVersion)
  )
import Distribution.PackageDescription
  ( GenericPackageDescription (packageDescription)
  , PackageDescription (package)
  )
import Distribution.PackageDescription.Parsec
  ( readGenericPackageDescription )
import Distribution.Pretty ( prettyShow )
import Distribution.Types.PackageName ( unPackageName )
import Distribution.Verbosity ( normal )
import Fmt ( (+|), (|+) )
import System.Directory ( getDirectoryContents )
import System.FilePath ( (</>) )

import HSInstall.Common ( stackClean )
import HSInstall.Except
  ( HSInstallException (NoCabalFiles)
  , throwM
  )
import HSInstall.Opts
  ( AppImageExe (getExe), Options (..)
  )


data DeploymentInfo = DeploymentInfo
  { prefixDir :: FilePath
  , binDir :: FilePath
  , shareDir :: FilePath
  , docDir :: FilePath
  , rsrcDir :: FilePath
  , version :: String
  }


constructDeploymentInfo :: Options -> IO DeploymentInfo
constructDeploymentInfo opts = do
  -- If we fail to find the cabal file, try again after a stack clean. If the
  -- project uses hpack, issuing any stack command will generate the cabal
  -- file.
  mbCabalFile <- getFirst <$>
       (First <$> locateCabalFile)
    <> (First <$> (stackClean >> locateCabalFile))
  maybe (throwM NoCabalFiles)
    (fmap (constructDeploymentInfo' opts . package . packageDescription)
      . readGenericPackageDescription normal) mbCabalFile


locateCabalFile :: IO (Maybe FilePath)
locateCabalFile = listToMaybe . filter (isSuffixOf ".cabal")
  <$> getDirectoryContents "."


constructDeploymentInfo' :: Options -> PackageId -> DeploymentInfo
constructDeploymentInfo' opts pkgId =
  DeploymentInfo prefixDir' binDir' shareDir'
    (shareDir' </> "doc") (shareDir' </> "resources") version'

  where
    prefixDir' = maybe (optPrefix opts) (\e -> (""+|getExe e|+".AppDir") </> "usr")
      $ optExecutable opts
    binDir' = prefixDir' </> "bin"
    project = unPackageName . pkgName $ pkgId
    version' = prettyShow . pkgVersion $ pkgId
    shareDir' = prefixDir' </> "share" </> (""+|project|+"-"+|version'|+"")
