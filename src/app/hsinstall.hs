{-# LANGUAGE OverloadedStrings #-}

import Control.Monad ( when )
import Control.Newtype.Generics ( op )
import qualified System.Directory as Dir
import System.Exit ( exitSuccess )
import System.FilePath ( (</>) )
import System.IO ( BufferMode (NoBuffering),
  hSetBuffering, stderr, stdout )
import Text.Printf ( printf )

import HSInstall.AppImage ( mkAppImage, prepAppImageFiles )
import HSInstall.Build ( BuildTool, clean,
  determineBuildTool, installBinaries )
import HSInstall.Common ( TmplDir (..), dumpStockIcon, tmplDir )
import HSInstall.DeploymentInfo
  ( BinDir (..)
  , DeploymentInfo (binDir, docDir, prefixDir)
  , DocDir (..)
  , PrefixDir (..)
  , constructDeploymentInfo
  )
import HSInstall.Except ( withExceptionHandling )
import HSInstall.Opts
  ( BuildMode (AppImageExe, Project), Options (..)
  , parseOpts
  )
import HSInstall.System.Directory ( copyTree )


main :: IO ()
main = do
  mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]
  withExceptionHandling $ do
    opts <- parseOpts

    buildTool <- determineBuildTool
    putStrLn $ "Build tool detected: " <> show buildTool

    when (optDumpIcon opts) $ dumpStockIcon Nothing >> exitSuccess

    di <- constructDeploymentInfo buildTool opts

    when (optClean opts) $ clean buildTool
    deployApplication buildTool (optBuildMode opts) di
    case optBuildMode opts of
      AppImageExe exe -> prepAppImageFiles exe >>= mkAppImage exe di
      Project         -> return ()


deployApplication :: BuildTool -> BuildMode -> DeploymentInfo -> IO ()
deployApplication buildTool mode di = do
  -- Copy the binaries
  let binFp = op BinDir . binDir $ di
  Dir.createDirectoryIfMissing True binFp
  installBinaries buildTool mode binFp

  -- Copy the license
  let licenseFile = "LICENSE"
  licenseFileExists <- Dir.doesFileExist licenseFile
  when licenseFileExists $ do
    printf "\nCopying %s\n" licenseFile
    let docFp = op DocDir . docDir $ di
    Dir.createDirectoryIfMissing True docFp
    Dir.copyFile licenseFile (docFp </> licenseFile)

  -- Copy the static template directory
  let tmplFp = op TmplDir tmplDir
  tmplExists <- Dir.doesDirectoryExist tmplFp
  when tmplExists $ do
    printf "\nCopying distribution files from template dir (%s)\n" tmplFp
    copyTree False tmplFp ((op PrefixDir) . prefixDir $ di)
