import Control.Monad ( unless, when )
import Data.List ( isSuffixOf )
import Distribution.Package
  ( PackageId
  , PackageIdentifier (pkgName, pkgVersion)
  )
import Distribution.PackageDescription
  ( GenericPackageDescription (packageDescription)
  , PackageDescription (package)
  )
import Distribution.PackageDescription.Parsec
  ( readGenericPackageDescription )
import Distribution.Pretty ( prettyShow )
import Distribution.Simple.Utils ( copyDirectoryRecursive )
import Distribution.Types.PackageName ( unPackageName )
import Distribution.Verbosity ( normal, silent, verbose )
import qualified System.Directory as Dir
import System.Environment ( getArgs )
import System.Exit ( ExitCode (ExitSuccess), die, exitSuccess )
import System.FilePath ( (</>) )
import System.Process ( system )
import Text.Printf ( printf )

import Opts ( Options (..), formattedVersion, parseOpts, usageText )


main :: IO ()
main = do
  -- Parse args
  (opts, _) <- parseOpts =<< getArgs

  -- User asked for help
  when (optHelp opts) $ usageText >>= putStrLn >> exitSuccess

  -- User asked for version
  when (optVersion opts) $ formattedVersion >>= putStrLn >> exitSuccess

  -- Locate cabal file
  cabalFiles <- (filter $ isSuffixOf ".cabal")
    <$> Dir.getDirectoryContents "."

  when (null cabalFiles) $ do
    die "Can't continue because no cabal files were found in ."

  -- Parse the cabal file and extract things we need from it
  -- then pass a pile of what we know to a function to create the
  -- installation dirs
  dirs <- constructDirs opts . package . packageDescription
    <$> readGenericPackageDescription normal (head cabalFiles)


  -- Perform the installation

  -- Remove existing install directory
  appDirExists <- Dir.doesDirectoryExist $ appDir dirs
  when (optDelete opts && appDirExists) $ do
    putStrLn $ "Removing existing directory " ++ (appDir dirs)
    Dir.removeDirectoryRecursive $ appDir dirs

  -- Clean before building
  when (optClean opts) $ system "stack clean" >> return ()

  -- Copy the binaries
  Dir.createDirectoryIfMissing True $ binDir dirs
  installExitCode <- system $ "stack install --local-bin-path=" ++ (binDir dirs)
  unless (installExitCode == ExitSuccess) $ die "Can't continue because stack install failed"

  -- Copy additional scripts
  {-
  putStrLn "Copying additional scripts"
  mapM_ (\f -> copyFile ("util" </> f) (binDir dirs </> f))
    [ "script1.sh", "script2.hs" ]
  -}

  -- Copy the license
  putStrLn "\nCopying LICENSE"
  Dir.createDirectoryIfMissing True $ docDir dirs
  Dir.copyFile "LICENSE" (docDir dirs </> "LICENSE")

  -- Copy the resources
  let rsrcDirSrc = "." </> "resources"
  rsrcsExist <- Dir.doesDirectoryExist rsrcDirSrc
  when rsrcsExist $ do
    putStrLn $ "\nCopying resources"
    copyTree (optRsrcCpVerbose opts) rsrcDirSrc (rsrcDir dirs)
    return ()

  exitSuccess


data Dirs = Dirs
  { appDir :: FilePath
  , binDir :: FilePath
  , docDir :: FilePath
  , rsrcDir :: FilePath
  }


constructDirs :: Options -> PackageId -> Dirs
constructDirs opts pkgId =
  Dirs appDir' binDir' (appDir' </> "doc") (appDir' </> "resources")

  where
    project = unPackageName . pkgName $ pkgId
    version' = prettyShow . pkgVersion $ pkgId
    appDir' = optPrefix opts </> "share" </> (printf "%s-%s" project version')
    binDir' = optPrefix opts </> "bin"


copyTree :: Bool -> FilePath -> FilePath -> IO ()
copyTree True  source target = copyDirectoryRecursive verbose source target
copyTree False source target = copyDirectoryRecursive silent  source target
