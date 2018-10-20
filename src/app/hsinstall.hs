import Control.Monad ( unless, when )
import Data.List ( isSuffixOf )
import Data.Maybe ( fromJust, isNothing )
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
import Distribution.Verbosity ( normal )
import qualified System.Directory as Dir
import System.Environment ( getArgs )
import System.Exit ( die, exitFailure, exitSuccess )
import System.FilePath ( (</>), (<.>), takeDirectory )
import System.Process ( callProcess )
import Text.Printf ( printf )

import HSInstall.Opts ( Options (..), formattedVersion, parseOpts, usageText )
import HSInstall.Resources ( getRsrcDir )
import Paths_hsinstall ( getDataDir )


main :: IO ()
main = do
  opts <- getOpts

  when (optDumpIcon opts) $ dumpStockIcon >> exitSuccess

  dirs <- constructDirs opts

  cleanup opts dirs
  deployApplication opts dirs
  when (optMkAppImage opts) $ mkAppImage opts dirs


getOpts :: IO Options
getOpts = do
  -- Parse args
  opts <- parseOpts =<< getArgs

  -- User asked for help
  when (optHelp opts) $ usageText >>= putStrLn >> exitSuccess

  -- User asked for version
  when (optVersion opts) $ formattedVersion >>= putStrLn >> exitSuccess

  when ((isNothing $ optExecutable opts) && optMkAppImage opts) $ do
    die "Can't continue because --mk-appimage is only possible when a single EXECUTABLE is specified"

  return opts


dumpStockIcon :: IO ()
dumpStockIcon = do
  resourcesDir <- getRsrcDir getDataDir
  let iconFilename = "unix-terminal" <.> "svg"
  let iconSourcePath = resourcesDir </> iconFilename

  iconFileExists <- Dir.doesFileExist iconSourcePath
  unless iconFileExists $ do
    printf "Error: icon file at this path is not present! %s\n" iconSourcePath
    exitFailure

  Dir.copyFile iconSourcePath iconFilename


data Dirs = Dirs
  { prefixDir :: FilePath
  , binDir :: FilePath
  , shareDir :: FilePath
  , docDir :: FilePath
  , rsrcDir :: FilePath
  }


constructDirs :: Options -> IO Dirs
constructDirs opts = do
  -- Locate cabal file
  cabalFiles <- (filter $ isSuffixOf ".cabal")
    <$> Dir.getDirectoryContents "."

  when (null cabalFiles) $ do
    die "Can't continue because no cabal files were found in ."

  -- Parse the cabal file and extract things we need from it
  -- then pass a pile of what we know to a function to create the
  -- installation dirs
  constructDirs' opts . package . packageDescription
    <$> readGenericPackageDescription normal (head cabalFiles)


constructDirs' :: Options -> PackageId -> Dirs
constructDirs' opts pkgId =
  Dirs prefixDir' binDir' shareDir'
    (shareDir' </> "doc") (shareDir' </> "resources")

  where
    prefixDir' = maybe (optPrefix opts) (\e -> "AppDir_" ++ e </> "usr")
      $ optExecutable opts
    binDir' = prefixDir' </> "bin"
    project = unPackageName . pkgName $ pkgId
    version' = prettyShow . pkgVersion $ pkgId
    shareDir' = prefixDir' </> "share" </> (printf "%s-%s" project version')


cleanup :: Options -> Dirs -> IO ()
cleanup opts dirs= do
  -- Remove existing application directory (the one down in PREFIX/share)
  shareDirExists <- Dir.doesDirectoryExist $ shareDir dirs
  when (optDelete opts && shareDirExists) $ do
    putStrLn $ "Removing existing directory " ++ (shareDir dirs)
    Dir.removeDirectoryRecursive $ shareDir dirs

  -- Clean before building
  when (optClean opts) $ callProcess "stack" ["clean"]


deployApplication :: Options -> Dirs -> IO ()
deployApplication opts dirs = do
  -- Copy the binaries
  Dir.createDirectoryIfMissing True $ binDir dirs
  callProcess "stack"
    [ "install", maybe "" (':' :) $ optExecutable opts
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
    printf "\nCopying %s\n" licenseFile
    Dir.createDirectoryIfMissing True $ docDir dirs
    Dir.copyFile licenseFile (docDir dirs </> licenseFile)

  -- Copy the resources
  let rsrcDirSrc = "." </> "resources"
  rsrcsExist <- Dir.doesDirectoryExist rsrcDirSrc
  when rsrcsExist $ do
    putStrLn $ "\nCopying resources"
    copyDirectoryRecursive normal rsrcDirSrc (rsrcDir dirs)


mkAppImage :: Options -> Dirs -> IO ()
mkAppImage opts dirs = do
  let appDir = takeDirectory $ prefixDir dirs
  let executable = fromJust $ optExecutable opts
  let appImageRsrcDir = "util" </> "resources" </> "appimage"
  callProcess "linuxdeploy-x86_64.AppImage"
    [ "--appdir=" ++ appDir
    , "--executable=" ++ (binDir dirs </> executable)
    , "--desktop-file=" ++ (appImageRsrcDir </> executable <.> "desktop")
    , "--icon-file=" ++ (appImageRsrcDir </> executable <.> "svg")
    , "--output=appimage"
    ]
