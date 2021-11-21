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
import Data.List ( find, isSuffixOf )
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
import System.Directory ( getDirectoryContents )
import System.FilePath ( (</>), (<.>) )

import HSInstall.Build ( BuildTool, makeCabal )
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
locateCabalFile = find (isSuffixOf ".cabal")
    <$> getDirectoryContents "."


constructDeploymentInfo :: BuildTool -> Options -> IO DeploymentInfo
constructDeploymentInfo buildTool opts = do
  mbCabalFile <- runMaybeT
    $   MaybeT locateCabalFile
    <|> MaybeT (makeCabal buildTool >> locateCabalFile)
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
    shareDir' = prefixDir' </> "share" </> project


defaultPrefix :: FilePath
defaultPrefix = "AppDir" </> "usr"

computePrefixDir :: Maybe FilePath -> BuildMode -> FilePath
computePrefixDir (Just prefix') _                 = prefix'
computePrefixDir Nothing        (AppImageExe exe) = exe <.> defaultPrefix
computePrefixDir Nothing        Project           = defaultPrefix
