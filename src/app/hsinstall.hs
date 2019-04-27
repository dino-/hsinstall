{-# LANGUAGE OverloadedStrings #-}

import Control.Monad ( when )
import Data.Maybe ( isNothing )
import Distribution.Simple.Utils ( copyDirectoryRecursive )
import Fmt ( (+|), (|+), fmtLn )
import qualified System.Directory as Dir
import System.Exit ( exitSuccess )
import System.FilePath ( (</>) )
import System.Process ( callProcess )

import HSInstall.AppImage ( mkAppImage, prepAppImageFiles )
import HSInstall.Common ( dumpStockIcon )
import HSInstall.DeploymentInfo
  ( DeploymentInfo (binDir, docDir, rsrcDir, shareDir)
  , constructDeploymentInfo, normal
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

  di <- constructDeploymentInfo opts

  let mbAppImageExe = optExecutable opts
  cleanup opts di
  deployApplication mbAppImageExe di
  maybe (return ()) (\aie ->
    when (optMkAppImage opts) $
      prepAppImageFiles aie >>= mkAppImage aie di
    ) mbAppImageExe


cleanup :: Options -> DeploymentInfo -> IO ()
cleanup opts di = do
  -- Remove existing application directory (the one down in PREFIX/share)
  shareDirExists <- Dir.doesDirectoryExist $ shareDir di
  when (optDelete opts && shareDirExists) $ do
    putStrLn $ "Removing existing directory " ++ shareDir di
    Dir.removeDirectoryRecursive $ shareDir di

  -- Clean before building
  when (optClean opts) $ callProcess "stack" ["clean"]


deployApplication :: Maybe AppImageExe -> DeploymentInfo -> IO ()
deployApplication mbAppImageExe di = do
  -- Copy the binaries
  Dir.createDirectoryIfMissing True $ binDir di
  callProcess "stack"
    [ "install", maybe "" (\aie -> ':' : getExe aie) mbAppImageExe
    , "--local-bin-path=" ++ binDir di
    ]

  -- Copy additional scripts
  {-
  putStrLn "Copying additional scripts"
  mapM_ (\f -> copyFile ("util" </> f) (binDir di </> f))
    [ "script1.sh", "script2.hs" ]
  -}

  -- Copy the license
  let licenseFile = "LICENSE"
  licenseFileExists <- Dir.doesFileExist licenseFile
  when licenseFileExists $ do
    fmtLn $ "\nCopying "+|licenseFile|+""
    Dir.createDirectoryIfMissing True $ docDir di
    Dir.copyFile licenseFile (docDir di </> licenseFile)

  -- Copy the resources
  let rsrcDirSrc = "." </> "resources"
  rsrcsExist <- Dir.doesDirectoryExist rsrcDirSrc
  when rsrcsExist $ do
    putStrLn "\nCopying resources"
    copyDirectoryRecursive normal rsrcDirSrc (rsrcDir di)
