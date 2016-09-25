#! /usr/bin/env runhaskell

{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception
import Control.Monad
import Data.List
import Data.Version
import Distribution.Package
import Distribution.PackageDescription hiding ( error, options )
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Distribution.Version
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process
import Text.Printf
import Text.Read


defaultPrefix :: FilePath
defaultPrefix = "/opt"


main :: IO ()
main = do
   -- Parse args
   (opts, args) <- parseOpts =<< getArgs

   -- User asked for help
   when (optHelp opts) $ putStrLn usageText >> exitSuccess

   unless (length args == 1) $ die usageText
   installType <- readInstallType $ head args

   -- Check for existence of the stack utility
   (flip unless $
      die "Can't continue because we can't find the stack utility") =<< stackExists

   -- Locate cabal file
   cabalFiles <- (filter $ isSuffixOf ".cabal") <$> getDirectoryContents "."

   when (null cabalFiles) $ do
      die "Can't continue because no cabal files were found in ."

   -- Parse the cabal file and extract things we need from it
   -- then pass a pile of what we know to a function to create the
   -- installation dirs
   dirs <- constructDirs installType opts . package . packageDescription
      <$> readPackageDescription normal (head cabalFiles)


   -- Perform the installation

   -- Remove existing install directory
   appDirExists <- doesDirectoryExist $ appDir dirs
   when (optDelete opts && appDirExists) $ do
      putStrLn $ "Removing existing directory " ++ (appDir dirs)
      removeDirectoryRecursive $ appDir dirs

   -- Clean before building
   when (optClean opts) $ system "stack clean" >> return ()

   -- Copy the binaries
   system $ "stack install --local-bin-path=" ++ (binDir dirs)

   -- Copy the license
   putStrLn "\nCopying LICENSE"
   createDirectoryIfMissing True $ docDir dirs
   copyFile "LICENSE" (docDir dirs </> "LICENSE")

   -- Copy the resources
   let rsrcDirSrc = "." </> "resources"
   rsrcsExist <- doesDirectoryExist rsrcDirSrc
   when rsrcsExist $ do
      putStrLn $ "\nCopying resources"
      copyTree (not . optQuietRsrc $ opts) rsrcDirSrc (rsrcDir dirs)
      return ()

   exitSuccess


data Dirs = Dirs
   { appDir :: FilePath
   , binDir :: FilePath
   , docDir :: FilePath
   , rsrcDir :: FilePath
   }


constructDirs :: InstallType -> Options -> PackageId -> Dirs
constructDirs instType opts pkgId =
   Dirs appDir binDir' (appDir </> "doc") (appDir </> "resources")

   where
      project = unPackageName . pkgName $ pkgId
      version = showVersion . pkgVersion $ pkgId
      versionPart = if optVersion opts then "-" ++ version else ""
      appDir = case instType of
         Bundle -> optPrefix opts </> (project ++ versionPart)
         FHS    -> optPrefix opts </> "share" </> (project ++ versionPart)
      binDir' = case instType of
         Bundle -> appDir </> "bin"
         FHS    -> optPrefix opts </> "bin"


stackExists :: IO Bool
stackExists = do
   result <- try $ readProcessWithExitCode "stack" ["--version"] ""
   return $ case result of
      Left  (_ :: IOException) -> False
      Right (ec, _, _)         -> ok ec


{- Turn an exit code (say, from system) into a Bool
-}
ok :: ExitCode -> Bool
ok ExitSuccess = True
ok _           = False


{-
   Argument parsing code
-}

data Options = Options
   { optClean :: Bool
   , optDelete :: Bool
   , optHelp :: Bool
   , optPrefix :: FilePath
   , optQuietRsrc :: Bool
   , optVersion :: Bool
   }

defaultOptions :: Options
defaultOptions = Options
   { optClean = True
   , optDelete = False
   , optHelp = False
   , optPrefix = defaultPrefix
   , optQuietRsrc = False
   , optVersion = True
   }


data InstallType = Bundle | FHS

instance Read InstallType where
   readsPrec _ "bundle" = [(Bundle, "")]
   readsPrec _ "fhs"    = [(FHS, "")]
   readsPrec _ _        = []


readInstallType :: String -> IO InstallType
readInstallType s =
   case (readEither s) of
      Left _ -> die $ printf "Can't continue because %s is not a valid install type\n\n%s" s usageText
      Right t -> return t


options :: [OptDescr (Options -> Options)]
options =
   [ Option ['C'] ["no-clean"]
      (NoArg (\opts -> opts { optClean = False } ))
      "Do not do 'stack clean' first"
   , Option ['d'] ["delete"]
      (NoArg (\opts -> opts { optDelete = True } ))
      "Delete the app directory before copying files"
   , Option ['h'] ["help"]
      (NoArg (\opts -> opts { optHelp = True } ))
      "This help information"
   , Option ['p'] ["prefix"]
      (ReqArg (\s opts -> opts { optPrefix = s } ) "PREFIX" )
      (printf "Install prefix directory. Defaults to %s so what you'll end up with is %s/PROJECT-VERSION" defaultPrefix defaultPrefix)
   , Option [] ["quiet-resources"]
      (NoArg (\opts -> opts { optQuietRsrc = True } ))
      "Don't be chatty when copying the resources directory. Useful when there are a LOT of resources."
   , Option ['V'] ["no-version"]
      (NoArg (\opts -> opts { optVersion = False } ))
      (printf "Do not include version in installation path, meaning: %s/PROJECT" defaultPrefix)
   ]


parseOpts :: [String] -> IO (Options, [String])
parseOpts args =
   case getOpt Permute options args of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError $ userError (concat errs ++ usageText)


usageText :: String
usageText = (usageInfo header options) ++ "\n" ++ footer
   where
      header = init $ unlines
         [ "Usage: install.hs [OPTIONS] INST_TYPE"
         , ""
         , "options:"
         ]
      footer = init $ unlines
         [ ""
         , "INST_TYPE is the topology used when copying files, one of: bundle, fhs"
         , ""
         , "bundle is sort-of a self-contained structure like this:"
         , ""
         , "  $PREFIX/"
         , "    $PROJECT-$VERSION/    <-- this is the \"app directory\""
         , "      bin/..."
         , "      doc/LICENSE"
         , "      resources/..."
         , ""
         , "fhs is the more traditional UNIX structure like this:"
         , ""
         , "  $PREFIX/"
         , "    bin/..."
         , "    share/"
         , "      $PROJECT-$VERSION/  <-- this is the \"app directory\""
         , "        doc/LICENSE"
         , "        resources/..."
         , ""
         , "Be aware that when the --delete switch is used along with fhs type, the binaries WILL NOT be deleted, only the \"app directory\"."
         , ""
         , "This script is part of the hsinstall package by Dino Morelli <dino@ui3.info>"
         ]


{-
   Recursive file copying code

   It was desireable to have a standalone recursive file copy in
   this script for maximum cross-platform compatibility and to
   avoid Haskell library dependencies.

   Many thanks to [abuzittin gillifirca](https://codereview.stackexchange.com/users/20251/abuzittin-gillifirca) for the StackOverflow post [Copying files in Haskell](https://codereview.stackexchange.com/questions/68908/copying-files-in-haskell) where the following code was lifted.
-}

copyTree chatty s t = do
    createDirectoryIfMissing True t
    subItems <- getSubitems s
    mapM_ (copyItem chatty s t) subItems


getSubitems :: FilePath -> IO [(Bool, FilePath)]
getSubitems path = getSubitems' ""
  where
    getChildren path =  (\\ [".", ".."]) <$> getDirectoryContents path

    getSubitems' relPath = do
        let absPath = path </> relPath
        isDir <- doesDirectoryExist absPath
        children <- if isDir then getChildren absPath else return []
        let relChildren = [relPath </> p | p <- children]
        ((isDir, relPath) :) . concat <$> mapM getSubitems' relChildren


copyItem chatty baseSourcePath baseTargetPath (isDir, relativePath) = do
    let sourcePath = baseSourcePath </> relativePath
    let targetPath = baseTargetPath </> relativePath

    when chatty $
       putStrLn $ "Copying " ++ sourcePath ++ " to " ++ targetPath

    if isDir
      then createDirectoryIfMissing False targetPath
      else copyFile sourcePath targetPath
