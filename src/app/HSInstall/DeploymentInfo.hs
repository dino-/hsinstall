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
  , prettyShow
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
import Distribution.Types.Version ( Version )
import Distribution.Verbosity ( normal )
import GHC.Generics hiding ( Prefix )
import System.Directory ( getDirectoryContents )
import System.FilePath ( (</>), (<.>) )

import HSInstall.Build ( BuildTool, makeCabal )
import HSInstall.Common ( ExePath (..) )
import HSInstall.Except
  ( HSInstallException (NoCabalFiles)
  , throwM
  )
import HSInstall.Opts
  ( BuildMode (AppImageExe, Project)
  , PrefixOpt (..)
  , Options (..)
  )


newtype PrefixDir = PrefixDir FilePath
  deriving Generic

instance Newtype PrefixDir

newtype BinDir = BinDir FilePath
  deriving Generic

instance Newtype BinDir

newtype DocDir = DocDir FilePath
  deriving Generic

instance Newtype DocDir

data DeploymentInfo = DeploymentInfo
  { prefixDir :: PrefixDir
  , binDir :: BinDir
  , docDir :: DocDir
  , version :: Version
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
  DeploymentInfo prefixDir' (BinDir binFp)
    (DocDir $ shareFp </> "doc") (pkgVersion pkgId)

  where
    prefixDir'@(PrefixDir prefixFp) = computePrefixDir (optPrefix opts) (optBuildMode opts)
    binFp = prefixFp </> "bin"
    project = unPackageName . pkgName $ pkgId
    shareFp = prefixFp </> "share" </> project


defaultPrefix :: PrefixDir
defaultPrefix = PrefixDir $ "AppDir" </> "usr"

computePrefixDir :: PrefixOpt -> BuildMode -> PrefixDir
computePrefixDir (Prefix prefixFp) _ = PrefixDir prefixFp
computePrefixDir NoPrefixSpecified (AppImageExe (ExePath exeFp)) =
  over PrefixDir (exeFp <.>) defaultPrefix
computePrefixDir NoPrefixSpecified Project = defaultPrefix
