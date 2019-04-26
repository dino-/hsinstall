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
import HSInstall.Common ( dumpStockIcon, stackClean )
import HSInstall.Dirs
  ( Dirs (binDir, docDir, rsrcDir, shareDir)
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
