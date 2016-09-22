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


defaultPrefix :: FilePath
defaultPrefix = "/opt"


main :: IO ()
main = do
   -- Parse args
   (opts, dirs) <- parseOpts =<< getArgs

   -- User asked for help
   when (optHelp opts) $ putStrLn usageText >> exitSuccess

   -- Check for existence of the stack utility
   (flip unless $
      die "Can't continue because we can't find the stack utility") =<< stackExists

   -- Locate cabal file
   cabalFiles <- (filter $ isSuffixOf ".cabal") <$> getDirectoryContents "."

   when (null cabalFiles) $ do
      die "Can't continue because no cabal files were found in ."

   -- Parse the cabal file and extract things we need from it
   pkg <- package . packageDescription <$> readPackageDescription normal (head cabalFiles)
   let project = unPackageName . pkgName $ pkg
   let version = showVersion . pkgVersion $ pkg

   -- Set the variables we need to proceed
   let prefix = if null dirs then defaultPrefix else head dirs
   let versionPart = if optVersion opts then "-" ++ version else ""
   let installDir = prefix </> (project ++ versionPart)


   -- Perform the installation

   -- Remove existing install directory
   installDirExists <- doesDirectoryExist installDir
   when (optDelete opts && installDirExists) $ do
      putStrLn $ "Removing existing directory " ++ installDir
      removeDirectoryRecursive installDir

   -- Clean before building
   when (optClean opts) $ system "stack clean" >> return ()

   -- Copy the binaries
   system $ "stack install --local-bin-path=" ++ (installDir </> "bin")

   -- Copy the license
   putStrLn "\nCopying LICENSE"
   let docDir = installDir </> "doc"
   createDirectoryIfMissing False docDir
   copyFile "LICENSE" (docDir </> "LICENSE")

   -- Copy the resources
   let rsrcDir = "." </> "resources"
   rsrcDirExists <- doesDirectoryExist rsrcDir
   when rsrcDirExists $ do
      putStrLn $ "\nCopying resources"
      copyTree rsrcDir (installDir </> "resources")
      return ()

   exitSuccess


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
   , optVersion :: Bool
   }

defaultOptions :: Options
defaultOptions = Options
   { optClean = True
   , optDelete = False
   , optHelp = False
   , optVersion = True
   }


options :: [OptDescr (Options -> Options)]
options =
   [ Option ['C'] ["no-clean"]
      (NoArg (\opts -> opts { optClean = False } ))
      "Do not do 'stack clean' first"
   , Option ['d'] ["delete"]
      (NoArg (\opts -> opts { optDelete = True } ))
      "Delete the dest directory before copying files"
   , Option ['h'] ["help"]
      (NoArg (\opts -> opts { optHelp = True } ))
      "This help information"
   , Option ['V'] ["no-version"]
      (NoArg (\opts -> opts { optVersion = False } ))
      "Do not include version in installation path"
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
         [ "Usage: hsinstall.hs [OPTIONS] [PREFIX]"
         , ""
         , "options:"
         ]
      footer = init $ unlines
         [ ""
         , printf "PREFIX is the install prefix directory. Defaults to %s so what you'll end up with is %s/PROJECT-VERSION" defaultPrefix defaultPrefix
         , ""
         , "Dino Morelli <dino@ui3.info>"
         ]


{-
   Recursive file copying code

   It was desireable to have a standalone recursive file copy in
   this script for maximum cross-platform compatibility and to
   avoid Haskell library dependencies.

   Many thanks to [abuzittin gillifirca](https://codereview.stackexchange.com/users/20251/abuzittin-gillifirca) for the StackOverflow post [Copying files in Haskell](https://codereview.stackexchange.com/questions/68908/copying-files-in-haskell) where the following code was lifted.
-}

copyTree s t = do
    createDirectoryIfMissing True t
    subItems <- getSubitems s
    mapM_ (copyItem s t) subItems


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


copyItem baseSourcePath baseTargetPath (isDir, relativePath) = do
    let sourcePath = baseSourcePath </> relativePath
    let targetPath = baseTargetPath </> relativePath

    putStrLn $ "Copying " ++ sourcePath ++ " to " ++ targetPath
    if isDir
      then createDirectoryIfMissing False targetPath
      else copyFile sourcePath targetPath
