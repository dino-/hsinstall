module Opts
  ( Options (..)
  , formattedVersion
  , parseOpts
  , usageText
  )
  where

import Data.Version ( showVersion )
import Paths_hsinstall ( version )
import System.Console.GetOpt
import System.Environment ( getProgName )
import Text.Printf ( printf )


defaultOptions :: Options
defaultOptions = Options
  { optClean = False
  , optDelete = False
  , optDumpIcon = False
  , optExecutable = Nothing
  , optHelp = False
  , optMkAppImage = False
  , optPrefix = "AppDir/usr"
  , optVersion = False
  }


data Options = Options
  { optClean :: Bool
  , optDelete :: Bool
  , optDumpIcon :: Bool
  , optExecutable :: Maybe String
  , optHelp :: Bool
  , optMkAppImage :: Bool
  , optPrefix :: FilePath
  , optVersion :: Bool
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
    "Save an icon for UNIX-like terminal apps to the current working directory. This is useful for making an AppImage for the first time. The file will be 'unix-terminal.svg'"
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


parseOpts :: [String] -> IO Options
parseOpts args =
  case getOpt Permute options args of
    (o,n,[]  ) -> do
      let oApplied = foldl (flip id) defaultOptions o
      let oWithExe = case n of
            (executable : _) -> oApplied { optExecutable = Just executable }
            _                -> oApplied
      return oWithExe
    (_,_,errs) -> do
      ut <- usageText
      ioError $ userError (concat errs ++ ut)


usageText :: IO String
usageText = do
  progName <- getProgName
  return $ (usageInfo (header progName) options) ++ "\n" ++ footer

  where
    header progName = init $ unlines
      [ "Usage: " ++ progName ++ " [OPTIONS] [EXECUTABLE]"
      , ""
      , "options:"
      ]
    footer = init $ unlines
      [ "INSTALLATION DIRECTORY TOPOLOGY"
      , ""
      , "The directory layout will be a traditional UNIX structure, also known as the FHS. Like this:"
      , ""
      , "  <PREFIX>/"
      , "    bin/..."
      , "    share/"
      , "      <PROJECT>-<VERSION>/  <-- this is the share directory"
      , "        doc/LICENSE"
      , "        resources/..."
      , ""
      , "Be aware that when the --delete switch is used the binaries in `<PREFIX>/bin` WILL NOT be deleted, only the share directory: <PREFIX>/share/<PROJECT>-<VERSION>"
      , ""
      , "Version " ++ (showVersion version) ++ "  Dino Morelli <dino@ui3.info>"
      ]


formattedVersion :: IO String
formattedVersion = do
  progName <- getProgName
  return $ printf "%s %s" progName (showVersion version)
