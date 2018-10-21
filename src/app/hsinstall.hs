import Control.Monad ( unless, when )
import Data.List ( isSuffixOf )
import Data.Maybe ( isNothing )
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
import System.Exit ( die, exitSuccess )
import System.FilePath ( (</>), (<.>), takeDirectory )
import System.Process ( callProcess )
import Text.Printf ( printf )

import HSInstall.Opts
  ( AppImageExe (getExe), Options (..)
  , formattedVersion, parseOpts, usageText
  )
import HSInstall.Resources ( getRsrcDir )
import Paths_hsinstall ( getDataDir )


main :: IO ()
main = do
  (opts, mbAppImageExe) <- getOpts

  when (optDumpIcon opts) $ dumpStockIcon Nothing >> exitSuccess

  dirs <- constructDirs opts mbAppImageExe

  cleanup opts dirs
  deployApplication mbAppImageExe dirs
  maybe (return ()) (\aie ->
    when (optMkAppImage opts) $
      prepAppImageFiles aie >>= mkAppImage aie dirs
    ) mbAppImageExe


getOpts :: IO (Options, Maybe AppImageExe)
getOpts = do
  -- Parse args
  allOpts@(opts, mbAppImageExe) <- parseOpts =<< getArgs

  -- User asked for help
  when (optHelp opts) $ usageText >>= putStrLn >> exitSuccess

  -- User asked for version
  when (optVersion opts) $ formattedVersion >>= putStrLn >> exitSuccess

  when (isNothing mbAppImageExe && optMkAppImage opts) $ do
    die "Can't continue because --mk-appimage is only possible when a single EXECUTABLE is specified"

  return allOpts


dumpStockIcon :: Maybe FilePath -> IO ()
dumpStockIcon mbDestPath = do
  resourcesDir <- getRsrcDir getDataDir
  let iconFilename = "unix-terminal" <.> "svg"
  let iconSourcePath = resourcesDir </> iconFilename

  iconFileExists <- Dir.doesFileExist iconSourcePath
  unless iconFileExists $ die $ printf
    "Error: icon file at this path is not present! %s\n" iconSourcePath

  let destPath = maybe iconFilename id mbDestPath

  Dir.copyFile iconSourcePath destPath


data Dirs = Dirs
  { prefixDir :: FilePath
  , binDir :: FilePath
  , shareDir :: FilePath
  , docDir :: FilePath
  , rsrcDir :: FilePath
  }


constructDirs :: Options -> Maybe AppImageExe -> IO Dirs
constructDirs opts mbAppImageExe = do
  -- Locate cabal file
  cabalFiles <- (filter $ isSuffixOf ".cabal")
    <$> Dir.getDirectoryContents "."

  when (null cabalFiles) $ do
    die "Can't continue because no cabal files were found in ."

  -- Parse the cabal file and extract things we need from it
  -- then pass a pile of what we know to a function to create the
  -- installation dirs
  constructDirs' opts mbAppImageExe . package . packageDescription
    <$> readGenericPackageDescription normal (head cabalFiles)


constructDirs' :: Options -> Maybe AppImageExe -> PackageId -> Dirs
constructDirs' opts mbAppImageExe pkgId =
  Dirs prefixDir' binDir' shareDir'
    (shareDir' </> "doc") (shareDir' </> "resources")

  where
    prefixDir' = maybe (optPrefix opts) (\e -> "AppDir_" ++ getExe e </> "usr")
      $ mbAppImageExe
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


deployApplication :: Maybe AppImageExe -> Dirs -> IO ()
deployApplication mbAppImageExe dirs = do
  -- Copy the binaries
  Dir.createDirectoryIfMissing True $ binDir dirs
  callProcess "stack"
    [ "install", maybe "" (\aie -> ':' : getExe aie) $ mbAppImageExe
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
  callProcess "linuxdeploy-x86_64.AppImage"
    [ "--appdir=" ++ (takeDirectory $ prefixDir dirs)
    , "--executable=" ++ (binDir dirs </> executable)
    , desktopArg
    , "--icon-file=" ++ (appImageRsrcDir </> executable <.> "svg")
    , "--output=appimage"
    ]
