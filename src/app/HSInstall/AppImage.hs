{-# LANGUAGE OverloadedStrings #-}

module HSInstall.AppImage
  ( mkAppImage, prepAppImageFiles )
  where

import Control.Monad ( unless )
import qualified System.Directory as Dir
import System.Environment ( setEnv )
import System.FilePath ( (</>), (<.>), takeDirectory )
import System.Process ( callProcess )

import HSInstall.Common ( dumpStockIcon, tmplDir )
import HSInstall.DeploymentInfo ( DeploymentInfo (binDir, prefixDir, version) )


data DesktopFileStatus = CreateNewDesktop | DesktopExists


desktopDir :: FilePath
desktopDir = tmplDir </> "share" </> "applications"

iconDir :: FilePath
iconDir = tmplDir </> "share" </> "icons" </> "hicolor" </> "scalable" </> "apps"


prepAppImageFiles :: String -> IO DesktopFileStatus
prepAppImageFiles exe = do
  -- Check and possibly create new icon
  let iconPath = iconDir </> exe <.> "svg"
  iconExists <- Dir.doesFileExist iconPath
  unless iconExists $ do
    Dir.createDirectoryIfMissing True iconDir
    dumpStockIcon $ Just iconPath

  -- Check desktop file, return status to caller
  let desktopPath = desktopDir </> exe <.> "desktop"
  desktopFileExists <- Dir.doesFileExist desktopPath
  return $ if desktopFileExists then DesktopExists else CreateNewDesktop


mkAppImage :: String -> DeploymentInfo -> DesktopFileStatus -> IO ()

mkAppImage exe di DesktopExists = do
  let desktopArg = "--desktop-file=" ++
        (desktopDir </> exe <.> "desktop")
  mkAppImage' exe di desktopArg

mkAppImage exe di CreateNewDesktop = do
  mkAppImage' exe di "--create-desktop-file"
  -- Now copy the freshly-created .desktop file into the project sources
  let desktopFile = exe <.> "desktop"
  Dir.createDirectoryIfMissing True desktopDir
  Dir.copyFile
    (prefixDir di </> "share" </> "applications" </> desktopFile)
    (desktopDir </> desktopFile)


mkAppImage' :: String -> DeploymentInfo -> String -> IO ()
mkAppImage' exe di desktopArg = do
  setEnv "VERSION" $ version di
  callProcess "linuxdeploy-x86_64.AppImage"
    [ "--appdir=" ++ (takeDirectory . prefixDir $ di)
    , "--executable=" ++ (binDir di </> exe)
    , desktopArg
    , "--icon-file=" ++ (iconDir </> exe <.> "svg")
    , "--output=appimage"
    ]
