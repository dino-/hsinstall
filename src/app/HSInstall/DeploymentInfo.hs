{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module HSInstall.DeploymentInfo
  ( BinDir (..)
  , DeploymentInfo (..)
  , DocDir (..)
  , PrefixDir (..)
  , constructDeploymentInfo

  -- re-exporting
  , normal
  )
  where

import Control.Applicative ( (<|>) )
import Control.Monad.Trans.Maybe ( MaybeT (..), runMaybeT )
import Control.Newtype.Generics
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
import GHC.Generics
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


newtype PrefixDir = PrefixDir FilePath
  deriving Generic

instance Newtype PrefixDir

newtype BinDir = BinDir FilePath
  deriving Generic

instance Newtype BinDir

newtype ShareDir = ShareDir FilePath

instance Newtype ShareDir FilePath where
  pack = ShareDir
  unpack (ShareDir fp) = fp

newtype DocDir = DocDir FilePath
  deriving Generic

instance Newtype DocDir

newtype RsrcDir = RsrcDir FilePath

instance Newtype RsrcDir FilePath where
  pack = RsrcDir
  unpack (RsrcDir fp) = fp

data DeploymentInfo = DeploymentInfo
  { prefixDir :: PrefixDir
  , binDir :: BinDir
  , shareDir :: ShareDir
  , docDir :: DocDir
  , rsrcDir :: RsrcDir
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
  DeploymentInfo (PrefixDir prefixFp) (BinDir binFp) (ShareDir shareFp)
    (DocDir $ shareFp </> "doc") (RsrcDir $ shareFp </> "resources") version'

  where
    prefixFp = computePrefixDir (optPrefix opts) (optBuildMode opts)
    binFp = prefixFp </> "bin"
    project = unPackageName . pkgName $ pkgId
    version' = prettyShow . pkgVersion $ pkgId
    shareFp = prefixFp </> "share" </> project


defaultPrefix :: FilePath
defaultPrefix = "AppDir" </> "usr"

computePrefixDir :: Maybe FilePath -> BuildMode -> FilePath
computePrefixDir (Just prefix') _                 = prefix'
computePrefixDir Nothing        (AppImageExe exe) = exe <.> defaultPrefix
computePrefixDir Nothing        Project           = defaultPrefix
