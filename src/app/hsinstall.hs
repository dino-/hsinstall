{-# LANGUAGE OverloadedStrings #-}

import Control.Monad ( when )
import Distribution.Simple.Utils ( copyDirectoryRecursive )
import Fmt ( (+|), (|+), fmtLn )
import qualified System.Directory as Dir
import System.Exit ( exitSuccess )
import System.FilePath ( (</>) )
import System.Process ( callProcess )

import HSInstall.AppImage ( mkAppImage, prepAppImageFiles )
import HSInstall.Common ( dumpStockIcon )
import HSInstall.DeploymentInfo
  ( DeploymentInfo (binDir, docDir, prefixDir, shareDir)
  , constructDeploymentInfo, normal
  )
import HSInstall.Except ( withExceptionHandling )
import HSInstall.Opts
  ( BuildMode (AppImageExe, Project), Options (..)
  , formattedVersion, parseOpts
  )


main :: IO ()
main = withExceptionHandling $ do
  opts <- parseOpts

  when (optVersion opts) $ formattedVersion >>= putStrLn >> exitSuccess

  when (optDumpIcon opts) $ dumpStockIcon Nothing >> exitSuccess

  di <- constructDeploymentInfo opts

  cleanup opts di
  deployApplication (optBuildMode opts) di
  case optBuildMode opts of
    AppImageExe exe -> prepAppImageFiles exe >>= mkAppImage exe di
    Project         -> return ()


cleanup :: Options -> DeploymentInfo -> IO ()
cleanup opts di = do
  -- Remove existing application directory (the one down in PREFIX/share)
  shareDirExists <- Dir.doesDirectoryExist $ shareDir di
  when (optDelete opts && shareDirExists) $ do
    putStrLn $ "Removing existing directory " ++ shareDir di
    Dir.removeDirectoryRecursive $ shareDir di

  -- Clean before building
  when (optClean opts) $ callProcess "stack" ["clean"]


modeToStackArg :: BuildMode -> String
modeToStackArg (AppImageExe exe) = ":"+|exe|+""
modeToStackArg Project           = ""


deployApplication :: BuildMode -> DeploymentInfo -> IO ()
deployApplication mode di = do
  -- Copy the binaries
  Dir.createDirectoryIfMissing True $ binDir di
  callProcess "stack"
    [ "install", modeToStackArg mode 
    , "--local-bin-path=" ++ binDir di
    ]

  -- Copy the license
  let licenseFile = "LICENSE"
  licenseFileExists <- Dir.doesFileExist licenseFile
  when licenseFileExists $ do
    fmtLn $ "\nCopying "+|licenseFile|+""
    Dir.createDirectoryIfMissing True $ docDir di
    Dir.copyFile licenseFile (docDir di </> licenseFile)

  -- Copy the static pack files
  let packDir = "." </> "pack"
  rsrcsExist <- Dir.doesDirectoryExist packDir
  when rsrcsExist $ do
    putStrLn "\nCopying static pack files"
    copyDirectoryRecursive normal packDir (prefixDir di)
