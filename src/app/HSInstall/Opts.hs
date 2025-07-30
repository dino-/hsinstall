{-# LANGUAGE DuplicateRecordFields, OverloadedStrings, QuasiQuotes #-}

module HSInstall.Opts
  ( BuildMode (..)
  , CleanSwitch (..)
  , DumpIconSwitch (..)
  , Options (..)
  , PrefixOpt (..)
  , parseOpts
  )
  where

import Data.Version (showVersion)
import Options.Applicative
import Paths_hsinstall (version)
import System.Environment (getProgName)
import Text.Heredoc (here)
import Text.PrettyPrint.ANSI.Leijen (string)
import Text.Printf (printf)

import HSInstall.Common (ExeFile (..), Signing (SigningKeyId, NoSignature))


newtype CleanSwitch = CleanSwitch { v :: Bool }

newtype DumpIconSwitch = DumpIconSwitch { v :: Bool }

data BuildMode = AppImageExe ExeFile | Project

data PrefixOpt = Prefix FilePath | NoPrefixSpecified

data Options = Options
  { optClean :: CleanSwitch
  , optDumpIcon :: DumpIconSwitch
  , optBuildMode :: BuildMode
  , optPrefix :: PrefixOpt
  , optSigning :: Signing
  }


parser :: Parser Options
parser = Options
  <$> ( CleanSwitch <$> switch
        (  long "clean"
        <> short 'c'
        <> help "Do stack or cabal 'clean' first"
        )
      )
  <*> ( DumpIconSwitch <$> switch
        (  long "dump-stock-icon"
        <> help "Save a default icon, unix-terminal.svg, to the current working directory"
        )
      )
  <*> option ( (AppImageExe . ExeFile) <$> str )
        (  long "mk-appimage"
        <> short 'i'
        <> metavar "EXE"
        <> help "Prepare the AppDir structure and build an AppImage for EXE. Changes PREFIX to EXE.AppDir/usr."
        <> value Project
        )
  <*> option ( Prefix <$> str )
        (  long "prefix"
        <> short 'p'
        <> metavar "DIR"
        <> help "Install prefix directory (Default: AppDir/usr)"
        <> value NoPrefixSpecified
        )
  <*> option ( SigningKeyId <$> str )
        (  long "sign"
        <> short 's'
        <> metavar "KEY_ID"
        <> help "Sign the AppImage with the specified GPG2 key id"
        <> value NoSignature
        )


versionHelper :: String -> Parser (a -> a)
versionHelper progName =
  infoOption (printf "%s %s" progName (showVersion version)) $ mconcat
  [ long "version"
  , help "Show version information"
  , hidden
  ]


parseOpts :: IO Options
parseOpts = do
  pn <- getProgName
  execParser $ info (parser <**> helper <**> versionHelper pn)
    (  header (printf "%s - Pack a haskell project into a deployable directory structure" pn)
    <> footer'
    )


footer' :: InfoMod a
footer' = footerDoc . Just . string . printf content . showVersion $ version
    where content = [here|OVERVIEW

hsinstall is a tool for installing a Haskell software project into a directory structure for deployment. It builds upon the `stack install` and `cabal install` commands and adds these features:

- Copies the `LICENSE` file into <PREFIX>/share/<PROJECT-NAME>/doc
- Copies the contents of a static directory stucture in your project (named `hsinstall`) into the destination prefix directory. This can contain additional binaries or scripts, resources, documentation, etc. (more on this later in TEMPLATE DIRECTORY)
- Optionally builds an AppDir directory structure for the project and produces an AppImage binary

To use hsinstall, it will be necessary to be in the top-level directory of a Haskell project that builds with either cabal or stack. You'll need to have one or the other of the cabal or stack tools on your path as well. Basically, if you can't build the project, hsinstall can't build it either.

Also note that all cabal commands will be issued as `v2-*` commands. The older `v1-*` commands are deprecated and we don't use them.

If the AppImage features are desired, you must have these tools on your PATH:
linuxdeploy: https://github.com/linuxdeploy/linuxdeploy/releases
linuxdeploy-plugin-appimage: https://github.com/linuxdeploy/linuxdeploy-plugin-appimage/releases

Running hsinstall on a project for the first time and with no arguments will produce this in . :

    AppDir/
      usr/
        bin/  <-- All binaries in the project
        share/
          <PROJECT-NAME>/
            doc/
              LICENSE

The -p,--prefix switch allows you to set a prefix other than `AppDir/usr`. This could be anywhere, like `myproject-2.3` or `/usr/local` or `/opt`

In addition, if an `hsinstall` directory exists, its contents will be copied into the prefix before build and install. See TEMPLATE DIRECTORY below for more info on this.

APPIMAGE CREATION

The -i,--mk-appimage switch will change the default prefix to `EXE.AppDir/usr` and only the specified EXE will be installed into `<PREFIX>/bin`, AppImages are intended to be made for exactly one binary.

If .desktop and .svg files are not found in the hsinstall directory, defaults will be created for you and placed in the correct subdirs. Check these files into source control for future builds.

The default `.desktop` file Categories will be populated with 'Utility;'. We recommend adjusting this using the XDG list of registered categories: https://specifications.freedesktop.org/menu-spec/latest/apa.html

If your application is a command-line program, append a line containing this to the end of the default `.desktop` file: 'Terminal=true'

If your application isn't a command-line program, we recommend using a proper icon instead of the hsinstall default, which is a command shell icon.

For more info on AppImage: https://appimage.org/

If you see failure like the following when using the -s,--sign switch:

    [appimage/stderr] [sign] gpgme_op_sign(gpgme_ctx, gpgme_appimage_file_data, gpgme_sig_data, GPGME_SIG_MODE_DETACH): call failed: Inappropriate ioctl for device

What's happening is gpg can't figure out a way to ask for the gpg key passphrase. Try setting the env like this

    $ GPG_TTY=$(tty) hsinstall ...


TEMPLATE DIRECTORY

If present, hsinstall will copy the contents of the `hsinstall` template directory into `<PREFIX>`. Here's an explanation of the hsinstall directory contents:

    hsinstall/
      bin/  <-- Put additional binaries and scripts to be deployed here
      share/
        applications/  <-- Only for AppImage
          <EXE>.desktop  <-- Will be generated by first-time AppImage creation attempt
        <PROJECT-NAME>/  <-- Only needed if you have resources
          resources/  <-- Put data files your software will need at runtime here
        icons/  <-- Only for AppImage
          hicolor/
            scalable/
              apps/
                <EXE>.svg  <-- Will be generated by first-time AppImage creation attempt

In order to locate data files at runtime, including resources, the hsinstall project includes a library to construct the share path relative to the executable. See this source code for help with integrating this into your app: https://github.com/dino-/hsinstall/blob/master/src/lib/HSInstall/Resources.hs

Version %s  Dino Morelli <dino@ui3.info>|]
