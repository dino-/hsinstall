{-# LANGUAGE OverloadedStrings #-}

module HSInstall.AppImage
  ( mkAppImage, prepAppImageFiles )
  where

import Control.Monad ( unless )
import Data.Version ( showVersion )
import Paths_hsinstall ( version )
import qualified System.Directory as Dir
import System.Environment ( setEnv )
import System.FilePath ( (</>), (<.>), takeDirectory )
import System.Process ( callProcess )

import HSInstall.Common ( dumpStockIcon )
import HSInstall.Dirs ( Dirs (binDir, prefixDir) )
import HSInstall.Opts ( AppImageExe (getExe) )


data DesktopFileStatus = CreateNewDesktop | DesktopExists


appImageRsrcDir :: FilePath
appImageRsrcDir = "util" </> "resources" </> "appimage"


prepAppImageFiles :: AppImageExe -> IO DesktopFileStatus
prepAppImageFiles appImageExe = do
  let exe = getExe appImageExe

  -- Check and possibly create new icon
  let iconPath = appImageRsrcDir </> exe <.> "svg"
  iconExists <- Dir.doesFileExist iconPath
  unless iconExists $ do
    Dir.createDirectoryIfMissing True appImageRsrcDir
    dumpStockIcon $ Just iconPath

  -- Check desktop file, return status to caller
  let desktopPath = appImageRsrcDir </> exe <.> "desktop"
  desktopFileExists <- Dir.doesFileExist desktopPath
  return $ if desktopFileExists then DesktopExists else CreateNewDesktop


mkAppImage :: AppImageExe -> Dirs -> DesktopFileStatus -> IO ()

mkAppImage appImageExe dirs DesktopExists = do
  let desktopArg = "--desktop-file=" ++
        (appImageRsrcDir </> getExe appImageExe <.> "desktop")
  mkAppImage' appImageExe dirs desktopArg

mkAppImage appImageExe dirs CreateNewDesktop = do
  mkAppImage' appImageExe dirs "--create-desktop-file"
  -- Now copy the freshly-created .desktop file into the project sources
  let desktopFile = getExe appImageExe <.> "desktop"
  Dir.copyFile
    (prefixDir dirs </> "share" </> "applications" </> desktopFile)
    (appImageRsrcDir </> desktopFile)


mkAppImage' :: AppImageExe -> Dirs -> String -> IO ()
mkAppImage' appImageExe dirs desktopArg = do
  let executable = getExe appImageExe
  setEnv "VERSION" $ showVersion version
  callProcess "linuxdeploy-x86_64.AppImage"
    [ "--appdir=" ++ (takeDirectory . prefixDir $ dirs)
    , "--executable=" ++ (binDir dirs </> executable)
    , desktopArg
    , "--icon-file=" ++ (appImageRsrcDir </> executable <.> "svg")
    , "--output=appimage"
    ]
