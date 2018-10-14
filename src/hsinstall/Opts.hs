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
  , optHelp = False
  , optPrefix = "AppDir/usr"
  , optRsrcCpVerbose = True
  , optVersion = False
  }


data Options = Options
  { optClean :: Bool
  , optDelete :: Bool
  , optHelp :: Bool
  , optPrefix :: FilePath
  , optRsrcCpVerbose :: Bool
  , optVersion :: Bool
  }


options :: [OptDescr (Options -> Options)]
options =
  [ Option ['c'] ["clean"]
    (NoArg (\opts -> opts { optClean = True } ))
    ("Do 'stack clean' first." ++ (defaultText . optClean $ defaultOptions))
  , Option ['d'] ["delete"]
    (NoArg (\opts -> opts { optDelete = True } ))
    ("Delete the app directory before copying files."
      ++ (defaultText . optDelete $ defaultOptions))
  , Option ['h'] ["help"]
    (NoArg (\opts -> opts { optHelp = True } ))
    "This help information."
  , Option ['p'] ["prefix"]
    (ReqArg (\s opts -> opts { optPrefix = s } ) "PREFIX" )
    (printf "Install prefix directory. Default: %s" (optPrefix defaultOptions))
  , Option ['R'] ["resource-copy-quiet"]
    (NoArg (\opts -> opts { optRsrcCpVerbose = False } ))
    ("Don't be chatty when copying the resources directory. Useful when there are a LOT of resources."
      ++ (defaultText . not . optRsrcCpVerbose $ defaultOptions))
  , Option [] ["version"]
    (NoArg (\opts -> opts { optVersion = True } ))
    "Show version information"
  ]


defaultText :: Bool -> String
defaultText True  = " Default"
defaultText False = ""


parseOpts :: [String] -> IO (Options, [String])
parseOpts args =
  case getOpt Permute options args of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> do
      ut <- usageText
      ioError $ userError (concat errs ++ ut)


usageText :: IO String
usageText = do
  progName <- getProgName
  return $ (usageInfo (header progName) options) ++ "\n" ++ footer

  where
    header progName = init $ unlines
      [ "Usage: " ++ progName ++ " [OPTIONS]"
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
      , "      <PROJECT>-<VERSION>/  <-- this is the \"app directory\""
      , "        doc/LICENSE"
      , "        resources/..."
      , ""
      , "Be aware that when the --delete switch is used the binaries in `<PREFIX>/bin` WILL NOT be deleted, only the \"app directory\"."
      , ""
      , "Version " ++ (showVersion version) ++ "  Dino Morelli <dino@ui3.info>"
      ]


formattedVersion :: IO String
formattedVersion = do
  progName <- getProgName
  return $ printf "%s %s" progName (showVersion version)
