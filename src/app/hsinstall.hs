{-# LANGUAGE OverloadedStrings #-}

import Control.Monad ( unless, when )
import Data.Maybe ( fromMaybe, isNothing )
import Data.Version ( showVersion )
import Distribution.Simple.Utils ( copyDirectoryRecursive )
import Fmt ( (+|), (|+), fmtLn )
import HSInstall.Resources ( getRsrcDir )
import Paths_hsinstall ( getDataDir, version )
import qualified System.Directory as Dir
import System.Environment ( setEnv )
import System.Exit ( exitSuccess )
import System.FilePath ( (</>), (<.>), takeDirectory )
import System.Process ( callProcess )

import HSInstall.Common ( stackClean )
import HSInstall.Dirs
  ( Dirs (binDir, docDir, prefixDir, rsrcDir, shareDir)
  , constructDirs, normal
  )
import HSInstall.Except
  ( HSInstallException (OneExePerAppImage)
  , withExceptionHandling, throwM
  )
import HSInstall.Opts
  ( AppImageExe (getExe), Options (..)
  , formattedVersion, parseOpts
  )


main :: IO ()
main = withExceptionHandling $ do
  opts <- parseOpts

  when (optVersion opts) $ formattedVersion >>= putStrLn >> exitSuccess

  when (isNothing (optExecutable opts) && optMkAppImage opts) $
    throwM OneExePerAppImage

  when (optDumpIcon opts) $ dumpStockIcon Nothing >> exitSuccess

  dirs <- constructDirs opts

  let mbAppImageExe = optExecutable opts
  cleanup opts dirs
  deployApplication mbAppImageExe dirs
  maybe (return ()) (\aie ->
    when (optMkAppImage opts) $
      prepAppImageFiles aie >>= mkAppImage aie dirs
    ) mbAppImageExe


dumpStockIcon :: Maybe FilePath -> IO ()
dumpStockIcon mbDestPath = do
  resourcesDir <- getRsrcDir getDataDir
  let iconFilename = "unix-terminal" <.> "svg"
  let iconSourcePath = resourcesDir </> iconFilename
  let destPath = fromMaybe iconFilename mbDestPath
  Dir.copyFile iconSourcePath destPath


cleanup :: Options -> Dirs -> IO ()
cleanup opts dirs= do
  -- Remove existing application directory (the one down in PREFIX/share)
  shareDirExists <- Dir.doesDirectoryExist $ shareDir dirs
  when (optDelete opts && shareDirExists) $ do
    putStrLn $ "Removing existing directory " ++ shareDir dirs
    Dir.removeDirectoryRecursive $ shareDir dirs

  -- Clean before building
  when (optClean opts) stackClean


deployApplication :: Maybe AppImageExe -> Dirs -> IO ()
deployApplication mbAppImageExe dirs = do
  -- Copy the binaries
  Dir.createDirectoryIfMissing True $ binDir dirs
  callProcess "stack"
    [ "install", maybe "" (\aie -> ':' : getExe aie) mbAppImageExe
    , "--local-bin-path=" ++ binDir dirs
    ]

  -- Copy additional scripts
  {-
  putStrLn "Copying additional scripts"
  mapM_ (\f -> copyFile ("util" </> f) (binDir dirs </> f))
    [ "script1.sh", "script2.hs" ]
  -}

  -- Copy the license
  let licenseFile = "LICENSE"
  licenseFileExists <- Dir.doesFileExist licenseFile
  when licenseFileExists $ do
    fmtLn $ "\nCopying "+|licenseFile|+""
    Dir.createDirectoryIfMissing True $ docDir dirs
    Dir.copyFile licenseFile (docDir dirs </> licenseFile)

  -- Copy the resources
  let rsrcDirSrc = "." </> "resources"
  rsrcsExist <- Dir.doesDirectoryExist rsrcDirSrc
  when rsrcsExist $ do
    putStrLn "\nCopying resources"
    copyDirectoryRecursive normal rsrcDirSrc (rsrcDir dirs)


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
