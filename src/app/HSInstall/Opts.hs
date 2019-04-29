{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module HSInstall.Opts
  ( BuildMode (..)
  , Options (..)
  , formattedVersion
  , parseOpts
  )
  where

import Data.Version ( showVersion )
import Fmt ( (+|), (|+), format )
import Options.Applicative
import Paths_hsinstall ( version )
import System.Environment ( getProgName )
import Text.Heredoc ( here )
import Text.PrettyPrint.ANSI.Leijen ( string )


data BuildMode = AppImageExe String | Project


data Options = Options
  { optClean :: Bool
  , optDelete :: Bool
  , optDumpIcon :: Bool
  , optBuildMode :: BuildMode
  , optPrefix :: FilePath
  , optVersion :: Bool
  }


parser :: Parser Options
parser = Options
  <$> switch
      (  long "clean"
      <> short 'c'
      <> help "Do 'stack clean' first"
      )
  <*> switch
      (  long "delete"
      <> short 'd'
      <> help "Delete the share directory before copying files"
      )
  <*> switch
      (  long "dump-stock-icon"
      <> help "Save a default icon, unix-termianl.svg, to the current working directory"
      )
  <*> ( maybe Project AppImageExe <$> optional
        ( strOption
          (  long "mk-appimage"
          <> short 'i'
          <> metavar "EXE"
          <> help "Prepare the AppDir structure and build an AppImage for EXE"
          )
        )
      )
  <*> strOption
      (  long "prefix"
      <> short 'p'
      <> showDefault
      <> value "AppDir/usr"
      <> metavar "PREFIX"
      <> help "Install prefix directory"
      )
  <*> switch
      (  long "version"
      <> help "Show version information"
      )


parseOpts :: IO Options
parseOpts = execParser $ info (parser <**> helper)
  (  header "hsinstall - Pack a haskell project into a deployable directory structure"
  <> footer'
  )


footer' :: InfoMod a
footer' = footerDoc . Just . string . format content . showVersion $ version
    where content = [here|OVERVIEW

hsinstall is a tool for deploying software projects into directory structures suitable for installation on a system. It builds upon the `stack install` command and adds these features:

- Copying the `LICENSE` file into the deployment directory
- Copying a static directory stucture (named `pack`) onto the destination prefix directory that can contain additional binaries or scripts, resources, documentation, etc. (more on this later in PACK DIRECTORY)
- Building an AppDir directory structure for a project and producing an AppImage

It will be necessary to have the Haskell stack tool on your PATH:
https://docs.haskellstack.org/en/stable/README/

If the AppImage features are desired, you must have these tools on your PATH:
linuxdeploy: https://github.com/linuxdeploy/linuxdeploy/releases
linuxdeploy-plugin-appimage: https://github.com/linuxdeploy/linuxdeploy-plugin-appimage/releases

MODES

hsinstall operates in two ways: build a distribution directory or build an AppImage.

If the -i,--mk-appimage switch is omitted, hsinstall will construct a distribution directory containing all binaries in the project, the license file and the contents of the `pack` directory if present. This could be used as the source for a distribution-specific packaging procedure or used as-is on a non-Linux system. The default prefix is `AppDir/usr`

The second mode includes everything above plus AppImage creation and is triggered by the -i,--mk-appimage switch. This will change the PREFIX to `<EXE>.AppDir/usr`. And only that single executable will be copied to the `<EXE>.AppDir/usr/bin` directory. AppImages are intended to contain exactly one binary each.

Regardless of which mode is being used, the directory layout will be a standard FHS shape, common in UNIX-like operating systems. Like this:

    <PREFIX>/
      bin/  <- stack will install your binaries here
      share/
        <PROJECT-NAME>/  <-- this is the share directory
          doc/LICENSE
          resources/  <-- Optional data files directory, see PACK DIRECTORY below

Be aware that when the `--delete` switch is used the binaries in `<PREFIX>/bin` WILL NOT be deleted, only the share directory: `<PREFIX>/share/<PROJECT-NAME>`

APPIMAGE CREATION

Even for a first-time AppImaging, this tool should produce a working AppImage. If missing, it will create default `.desktop` and `.svg` files in `pack/share`. Customize or replace these to fit your project, and then check these two files into source control for future builds. For more info, see PACK DIRECTORY below.

The default `.desktop` file Categories will be populated with 'Utility;'. We recommend adjusting this using the XDG list of registered categories: https://specifications.freedesktop.org/menu-spec/latest/apa.html

If your application is a command-line program, append a line containing this to the end of the default `.desktop` file: 'Terminal=true'

If your application isn't a command-line tool, we recommend using a proper icon instead of the hsinstall default, which is a command shell icon.

PACK DIRECTORY

If present, hsinstall will copy the contents of the `pack` directory onto `<PREFIX>`. Here's a sample of a fully-featured pack directory:

    pack/
      bin/  <-- Put additional binaries and scripts to be deployed here
      share/
        applications/  <-- Only for AppImage
          <EXE>.desktop  <-- Will be generated by first-time AppImage creation attempt
        <PROJECT-NAME>/  <-- Only needed if you have resources
          resources/  <-- Put data files the software will need at runtime here
        icons/  <-- Only for AppImage
          hicolor/
            scalable/
              apps/
                <EXE>.svg  <-- Will be generated by first-time AppImage creation attempt

In order to locate the resources files at runtime, the hsinstall project includes a library to construct paths relative to the executable. See this source code for help with integrating this into your app: https://github.com/dino-/hsinstall/blob/master/src/lib/HSInstall/Resources.hs

Version {}  Dino Morelli <dino@ui3.info>|]


formattedVersion :: IO String
formattedVersion = do
  progName <- getProgName
  return (""+|progName|+" "+|showVersion version|+"")
