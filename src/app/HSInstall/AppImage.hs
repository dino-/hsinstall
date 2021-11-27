{-# LANGUAGE OverloadedStrings #-}

module HSInstall.AppImage
  ( mkAppImage, prepAppImageFiles )
  where

import Control.Monad ( unless )
import Control.Newtype.Generics ( op )
import qualified System.Directory as Dir
import System.Environment ( setEnv )
import System.FilePath ( (</>), (<.>), takeDirectory )
import System.Process ( callProcess )

import HSInstall.Common ( ExePath (..), TmplDir (..), dumpStockIcon
  , tmplDir )
import HSInstall.DeploymentInfo
  ( BinDir (..)
  , PrefixDir (..)
  , DeploymentInfo (binDir, prefixDir, version)
  , prettyShow
  )


data DesktopFileStatus = CreateNewDesktop | DesktopExists


desktopDir :: FilePath
desktopDir = op TmplDir tmplDir </> "share" </> "applications"

iconDir :: FilePath
iconDir = op TmplDir tmplDir </> "share" </> "icons" </> "hicolor" </> "scalable" </> "apps"


prepAppImageFiles :: ExePath -> IO DesktopFileStatus
prepAppImageFiles (ExePath exeFp) = do
  -- Check and possibly create new icon
  let iconPath = iconDir </> exeFp <.> "svg"
  iconExists <- Dir.doesFileExist iconPath
  unless iconExists $ do
    Dir.createDirectoryIfMissing True iconDir
    dumpStockIcon $ Just iconPath

  -- Check desktop file, return status to caller
  let desktopPath = desktopDir </> exeFp <.> "desktop"
  desktopFileExists <- Dir.doesFileExist desktopPath
  return $ if desktopFileExists then DesktopExists else CreateNewDesktop


newtype Arg = Arg String

mkAppImage :: ExePath -> DeploymentInfo -> DesktopFileStatus -> IO ()

mkAppImage exePath di DesktopExists = do
  let desktopArg = Arg ("--desktop-file=" <>
        (desktopDir </> op ExePath exePath <.> "desktop"))
  mkAppImage' exePath di desktopArg

mkAppImage exePath di CreateNewDesktop = do
  mkAppImage' exePath di (Arg "--create-desktop-file")
  -- Now copy the freshly-created .desktop file into the project sources
  let desktopFile = op ExePath exePath <.> "desktop"
  Dir.createDirectoryIfMissing True desktopDir
  Dir.copyFile
    ((op PrefixDir . prefixDir $ di) </> "share" </> "applications" </> desktopFile)
    (desktopDir </> desktopFile)


mkAppImage' :: ExePath -> DeploymentInfo -> Arg -> IO ()
mkAppImage' (ExePath exeFp) di (Arg desktopArg) = do
  setEnv "VERSION" (prettyShow . version $ di)
  callProcess "linuxdeploy-x86_64.AppImage"
    [ "--appdir=" ++ (takeDirectory . op PrefixDir . prefixDir $ di)
    , "--executable=" <> ((op BinDir . binDir $ di) </> exeFp)
    , desktopArg
    , "--icon-file=" ++ (iconDir </> exeFp <.> "svg")
    , "--output=appimage"
    ]
