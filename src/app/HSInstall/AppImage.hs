{-# LANGUAGE OverloadedRecordDot, OverloadedStrings #-}

module HSInstall.AppImage
  ( mkAppImage, prepAppImageFiles )
  where

import Control.Monad ( unless )
import qualified System.Directory as Dir
import System.Environment ( setEnv )
import System.FilePath ( (</>), (<.>), takeDirectory )
import System.Process ( callProcess )

import HSInstall.Common ( ExeFile (..), TmplDir (v), dumpStockIcon
  , tmplDir )
import HSInstall.DeploymentInfo
  ( BinDir (..)
  , PrefixDir (..)
  , DeploymentInfo (binDir, prefixDir, version)
  , prettyShow
  )


data DesktopFileStatus = CreateNewDesktop | DesktopExists


desktopDir :: FilePath
desktopDir = tmplDir.v </> "share" </> "applications"

iconDir :: FilePath
iconDir = tmplDir.v </> "share" </> "icons" </> "hicolor" </> "scalable" </> "apps"


prepAppImageFiles :: ExeFile -> IO DesktopFileStatus
prepAppImageFiles (ExeFile exeFp) = do
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

mkAppImage :: ExeFile -> DeploymentInfo -> DesktopFileStatus -> IO ()

mkAppImage exeFile di DesktopExists = do
  let desktopArg = Arg ("--desktop-file=" <>
        (desktopDir </> exeFile.v <.> "desktop"))
  mkAppImage' exeFile di desktopArg

mkAppImage exeFile di CreateNewDesktop = do
  mkAppImage' exeFile di (Arg "--create-desktop-file")
  -- Now copy the freshly-created .desktop file into the project sources
  let desktopFile = exeFile.v <.> "desktop"
  Dir.createDirectoryIfMissing True desktopDir
  Dir.copyFile
    (di.prefixDir.v </> "share" </> "applications" </> desktopFile)
    (desktopDir </> desktopFile)


mkAppImage' :: ExeFile -> DeploymentInfo -> Arg -> IO ()
mkAppImage' (ExeFile exeFp) di (Arg desktopArg) = do
  setEnv "LINUXDEPLOY_OUTPUT_VERSION" (prettyShow . version $ di)
  callProcess "linuxdeploy-x86_64.AppImage"
    [ "--appdir=" ++ takeDirectory di.prefixDir.v
    , "--executable=" <> (di.binDir.v </> exeFp)
    , desktopArg
    , "--icon-file=" ++ (iconDir </> exeFp <.> "svg")
    , "--output=appimage"
    ]
