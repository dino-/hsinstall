{-# LANGUAGE OverloadedRecordDot, OverloadedStrings #-}

import Control.Monad (when)
import qualified System.Directory as Dir
import System.Exit (exitSuccess)
import System.FilePath ((</>))
import System.IO (BufferMode (NoBuffering),
  hSetBuffering, stderr, stdout)
import Text.Printf (printf)

import HSInstall.AppImage (mkAppImage, prepAppImageFiles)
import HSInstall.Build (BuildTool, clean,
  determineBuildTool, installBinaries)
import HSInstall.Common (TmplDir (v), dumpStockIcon, tmplDir)
import HSInstall.DeploymentInfo
  ( BinDir (..)
  , DeploymentInfo (binDir, docDir, prefixDir)
  , DocDir (..)
  , PrefixDir (..)
  , constructDeploymentInfo
  )
import HSInstall.Except (withExceptionHandling)
import HSInstall.Opts
  ( BuildMode (AppImageExe, Project)
  , CleanSwitch (..), DumpIconSwitch (..)
  , Options (..)
  , parseOpts
  )
import HSInstall.System.Directory (copyTree)


main :: IO ()
main = do
  mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]
  withExceptionHandling $ do
    opts <- parseOpts

    buildTool <- determineBuildTool
    putStrLn $ "Build tool detected: " <> show buildTool

    when opts.optDumpIcon.v $ dumpStockIcon Nothing >> exitSuccess

    di <- constructDeploymentInfo buildTool opts

    when opts.optClean.v $ clean buildTool
    deployApplication buildTool (optBuildMode opts) di
    case optBuildMode opts of
      AppImageExe exePath -> prepAppImageFiles exePath >>= mkAppImage (optSigning opts) exePath di
      Project             -> return ()


deployApplication :: BuildTool -> BuildMode -> DeploymentInfo -> IO ()
deployApplication buildTool mode di = do
  -- Copy the binaries
  let binFp = di.binDir.v
  Dir.createDirectoryIfMissing True binFp
  installBinaries buildTool mode binFp

  -- Copy the license
  let licenseFile = "LICENSE"
  licenseFileExists <- Dir.doesFileExist licenseFile
  when licenseFileExists $ do
    printf "\nCopying %s\n" licenseFile
    let docFp = di.docDir.v
    Dir.createDirectoryIfMissing True docFp
    Dir.copyFile licenseFile (docFp </> licenseFile)

  -- Copy the static template directory
  let tmplFp = tmplDir.v
  tmplExists <- Dir.doesDirectoryExist tmplFp
  when tmplExists $ do
    printf "\nCopying distribution files from template dir (%s)\n" tmplFp
    copyTree False tmplFp di.prefixDir.v
