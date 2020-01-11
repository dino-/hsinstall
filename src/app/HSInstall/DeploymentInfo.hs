{-# LANGUAGE OverloadedStrings #-}

module HSInstall.DeploymentInfo
  ( DeploymentInfo (..)
  , constructDeploymentInfo

  -- re-exporting
  , normal
  )
  where

import Control.Applicative ( (<|>) )
import Control.Monad.Trans.Maybe ( MaybeT (..), runMaybeT )
import Data.List ( isSuffixOf )
import Data.Maybe ( listToMaybe )
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
import System.Process ( callProcess )

import HSInstall.Except
  ( HSInstallException (NoCabalFiles)
  , throwM
  )
import HSInstall.Opts
  ( BuildMode (AppImageExe, Project) , Options (..)
  )


data DeploymentInfo = DeploymentInfo
  { prefixDir :: FilePath
  , binDir :: FilePath
  , shareDir :: FilePath
  , docDir :: FilePath
  , rsrcDir :: FilePath
  , version :: String
  }


locateCabalFile :: IO (Maybe FilePath)
locateCabalFile = listToMaybe . filter (isSuffixOf ".cabal")
    <$> getDirectoryContents "."


constructDeploymentInfo :: Options -> IO DeploymentInfo
constructDeploymentInfo opts = do
  mbCabalFile <- runMaybeT
    $   MaybeT (locateCabalFile)
    <|> MaybeT (callProcess "stack" ["query"] >> locateCabalFile)
  maybe (throwM NoCabalFiles)
    (fmap (constructDeploymentInfo' opts . package . packageDescription)
      . readGenericPackageDescription normal) mbCabalFile


constructDeploymentInfo' :: Options -> PackageId -> DeploymentInfo
constructDeploymentInfo' opts pkgId =
  DeploymentInfo prefixDir' binDir' shareDir'
    (shareDir' </> "doc") (shareDir' </> "resources") version'

  where
    prefixDir' = computePrefixDir (optPrefix opts) (optBuildMode opts)
    binDir' = prefixDir' </> "bin"
    project = unPackageName . pkgName $ pkgId
    version' = prettyShow . pkgVersion $ pkgId
    shareDir' = prefixDir' </> "share" </> (""+|project|+"")


computePrefixDir :: Maybe FilePath -> BuildMode -> FilePath
computePrefixDir (Just prefix') _                 = prefix'
computePrefixDir Nothing        (AppImageExe exe) = (""+|exe|+".AppDir") </> "usr"
computePrefixDir Nothing        Project           = "AppDir" </> "usr"
