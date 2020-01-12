{-# LANGUAGE OverloadedStrings #-}

import Control.Monad ( when )
import qualified System.Directory as Dir
import System.Exit ( exitSuccess )
import System.FilePath ( (</>) )
import System.Process ( callProcess )
import Text.Printf ( printf )

import HSInstall.AppImage ( mkAppImage, prepAppImageFiles )
import HSInstall.Common ( dumpStockIcon, tmplDir )
import HSInstall.DeploymentInfo
  ( DeploymentInfo (binDir, docDir, prefixDir)
  , constructDeploymentInfo
  )
import HSInstall.Except ( withExceptionHandling )
import HSInstall.Opts
  ( BuildMode (AppImageExe, Project), Options (..)
  , parseOpts
  )
import HSInstall.System.Directory ( copyTree )


main :: IO ()
main = withExceptionHandling $ do
  opts <- parseOpts

  when (optDumpIcon opts) $ dumpStockIcon Nothing >> exitSuccess

  di <- constructDeploymentInfo opts

  when (optClean opts) $ callProcess "stack" ["clean"]
  deployApplication (optBuildMode opts) di
  case optBuildMode opts of
    AppImageExe exe -> prepAppImageFiles exe >>= mkAppImage exe di
    Project         -> return ()


modeToStackArg :: BuildMode -> String
modeToStackArg (AppImageExe exe) = ':' : exe
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
    printf "\nCopying %s\n" licenseFile
    Dir.createDirectoryIfMissing True $ docDir di
    Dir.copyFile licenseFile (docDir di </> licenseFile)

  -- Copy the static template directory
  tmplExists <- Dir.doesDirectoryExist tmplDir
  when tmplExists $ do
    printf "\nCopying distribution files from template dir (%s)\n" tmplDir
    copyTree False tmplDir (prefixDir di)
