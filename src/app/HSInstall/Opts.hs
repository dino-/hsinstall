{-# LANGUAGE QuasiQuotes #-}

module HSInstall.Opts
  ( AppImageExe (getExe)
  , Options (..)
  , formattedVersion
  , parseOpts
  , usageText
  )
  where

import Data.Maybe ( listToMaybe )
import Data.String.Here.Interpolated ( iTrim )
import Data.Version ( showVersion )
import Paths_hsinstall ( version )
import System.Console.GetOpt
import System.Environment ( getProgName )
import Text.Printf ( printf )


newtype AppImageExe = AppImageExe { getExe :: String }


data Options = Options
  { optClean :: Bool
  , optDelete :: Bool
  , optDumpIcon :: Bool
  , optHelp :: Bool
  , optMkAppImage :: Bool
  , optPrefix :: FilePath
  , optVersion :: Bool
  }


defaultOptions :: Options
defaultOptions = Options
  { optClean = False
  , optDelete = False
  , optDumpIcon = False
  , optHelp = False
  , optMkAppImage = False
  , optPrefix = "AppDir/usr"
  , optVersion = False
  }


options :: [OptDescr (Options -> Options)]
options =
  [ Option ['c'] ["clean"]
    (NoArg (\opts -> opts { optClean = True } ))
    "Do 'stack clean' first"
  , Option ['d'] ["delete"]
    (NoArg (\opts -> opts { optDelete = True } ))
    "Delete the share directory before copying files"
  , Option [] ["dump-stock-icon"]
    (NoArg (\opts -> opts { optDumpIcon = True } ))
    "Save a default icon, unix-termianl.svg, to the current working directory"
  , Option ['h'] ["help"]
    (NoArg (\opts -> opts { optHelp = True } ))
    "This help information"
  , Option ['i'] ["mk-appimage"]
    (NoArg (\opts -> opts { optMkAppImage = True } ))
    "Prepare the AppDir structure and build an AppImage for EXECUTABLE"
  , Option ['p'] ["prefix"]
    (ReqArg (\s opts -> opts { optPrefix = s } ) "PREFIX" )
    (printf "Install prefix directory. Default: %s" (optPrefix defaultOptions))
  , Option [] ["version"]
    (NoArg (\opts -> opts { optVersion = True } ))
    "Show version information"
  ]


parseOpts :: [String] -> IO (Options, Maybe AppImageExe)
parseOpts args =
  case getOpt Permute options args of
    (o,n,[]  ) -> do
      let oApplied = foldl (flip id) defaultOptions o
      return (oApplied, AppImageExe <$> listToMaybe n)
    (_,_,errs) -> do
      ut <- usageText
      ioError $ userError (concat errs ++ ut)


usageText :: IO String
usageText = do
  progName <- getProgName
  return $ (usageInfo (header progName) options) ++ "\n" ++ body

  where
    header progName = init $ unlines
      [ "Usage: " ++ progName ++ " [OPTIONS]"
      , "       " ++ progName ++ " [OPTIONS] [EXECUTABLE]"
      , ""
      , "options:"
      ]
    body = [iTrim|
OVERVIEW

hsinstall is a tool for deploying software projects into directory structures suitable for installation on a system. It builds upon the `stack install` command and adds more features. Those are:

- Copying the `LICENSE` file into the deployment directory
- Copying the `resources` directory into the deployment directory so these files can be located using relative paths at runtime (more on this later in RESOURCES)
- Building an AppDir directory structure for a project and producing an AppImage

It will be necessary to have the Haskell stack tool on your PATH:
https://docs.haskellstack.org/en/stable/README/

If the AppImage features are desired, you must have these tools on your PATH:
linuxdeploy: https://github.com/linuxdeploy/linuxdeploy/releases
linuxdeploy-plugin-appimage: https://github.com/linuxdeploy/linuxdeploy-plugin-appimage/releases

MODES

hsinstall operates in two modes. The first is a plain deployment with no AppImage creation. The PREFIX will default to `AppDir/usr` and all binaries in the project will be deployed to `AppDir/usr/bin`.

The second mode is intended to set up for AppImage creation and is triggered by specifying exactly one EXECUTABLE from the project in the arguments. This will change the PREFIX to `AppDir_EXECUTABLE/usr`. And only that single executable will be copied to the `AppDir_EXECUTABLE/usr/bin` directory.

The directory layout will be a standard FHS shape, common in UNIX-like operating systems. Like this:

    <PREFIX>/
      bin/...
      share/
        <PROJECT>-<VERSION>/  <-- this is the share directory
          doc/LICENSE
          resources/...

Be aware that when the `--delete` switch is used the binaries in `<PREFIX>/bin` WILL NOT be deleted, only the share directory: `<PREFIX>/share/<PROJECT>-<VERSION>`

APPIMAGE CREATION

Even for a first-time AppImaging, this tool should produce a working AppImage. If missing, it will create default `.desktop` and `.svg` files in `util/resources/appimage`. Customize or replace these to fit your project, and then check these two files into source control for future builds.

The default `.desktop` file Categories will be populated with 'Utility;'. We recommend adjusting this using the XDG list of registered categories: https://specifications.freedesktop.org/menu-spec/latest/apa.html

If your application is a command-line program, append this line to the end of the default `.desktop` file: 'Terminal=true'

If your application isn't a command-line tool, we recommend using a proper icon instead of the hsinstall default, which is a command shell icon.

RESOURCES

If present, hsinstall will deploy a `resources` directory to `<PREFIX>/share/PROJECT-VERSION/resources`. In order to locate these files at runtime, the hsinstall project includes a library to build filesystem-portable relative paths. See this source code for help on integrating this into your app: https://github.com/dino-/hsinstall/blob/master/src/lib/HSInstall/Resources.hs


Version ${showVersion version}  Dino Morelli <dino@ui3.info>
|]


formattedVersion :: IO String
formattedVersion = do
  progName <- getProgName
  return $ printf "%s %s" progName (showVersion version)
