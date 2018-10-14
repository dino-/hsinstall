import Control.Monad
import Data.List
import Data.Version ( showVersion )
import Distribution.Package
import Distribution.PackageDescription hiding ( options )
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Paths_hsinstall ( version )
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process
import Text.Printf


defaultOptions :: Options
defaultOptions = Options
   { optClean = False
   , optDelete = False
   , optHelp = False
   , optPrefix = "AppDir/usr"
   , optRsrcCpVerbose = True
   -- , optVersion = True
   }


main :: IO ()
main = do
   -- Parse args
   (opts, _) <- parseOpts =<< getArgs

   -- User asked for help
   when (optHelp opts) $ usageText >>= putStrLn >> exitSuccess

   -- Locate cabal file
   cabalFiles <- (filter $ isSuffixOf ".cabal") <$> getDirectoryContents "."

   when (null cabalFiles) $ do
      die "Can't continue because no cabal files were found in ."

   -- Parse the cabal file and extract things we need from it
   -- then pass a pile of what we know to a function to create the
   -- installation dirs
   dirs <- constructDirs opts . package . packageDescription
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
   createDirectoryIfMissing True $ binDir dirs
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
   createDirectoryIfMissing True $ docDir dirs
   copyFile "LICENSE" (docDir dirs </> "LICENSE")

   -- Copy the resources
   let rsrcDirSrc = "." </> "resources"
   rsrcsExist <- doesDirectoryExist rsrcDirSrc
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
      version' = showVersion . pkgVersion $ pkgId
      appDir' = optPrefix opts </> "share" </> (printf "%s-%s" project version')
      binDir' = optPrefix opts </> "bin"


{-
   Argument parsing code
-}

data Options = Options
   { optClean :: Bool
   , optDelete :: Bool
   , optHelp :: Bool
   , optPrefix :: FilePath
   , optRsrcCpVerbose :: Bool
   -- , optVersion :: Bool
   }


options :: [OptDescr (Options -> Options)]
options =
   [ Option ['c'] ["clean"]
      (NoArg (\opts -> opts { optClean = True } ))
      ("Do 'stack clean' first." ++ (defaultText . optClean $ defaultOptions))
   , Option ['C'] ["no-clean"]
      (NoArg (\opts -> opts { optClean = False } ))
      ("Do not 'stack clean' first."
         ++ (defaultText . not . optClean $ defaultOptions))
   , Option ['d'] ["delete"]
      (NoArg (\opts -> opts { optDelete = True } ))
      ("Delete the app directory before copying files."
         ++ (defaultText . optDelete $ defaultOptions))
   , Option ['D'] ["no-delete"]
      (NoArg (\opts -> opts { optDelete = False } ))
      ("Do not delete the app directory before copying files."
         ++ (defaultText . not . optDelete $ defaultOptions))
   , Option ['h'] ["help"]
      (NoArg (\opts -> opts { optHelp = True } ))
      "This help information."
   , Option ['p'] ["prefix"]
      (ReqArg (\s opts -> opts { optPrefix = s } ) "PREFIX" )
      (printf "Install prefix directory. Default: %s" (optPrefix defaultOptions))
   , Option ['r'] ["resource-copy-verbose"]
      (NoArg (\opts -> opts { optRsrcCpVerbose = True } ))
      ("Be chatty when copying the resources directory."
         ++ (defaultText . optRsrcCpVerbose $ defaultOptions))
   , Option ['R'] ["no-resource-copy-verbose"]
      (NoArg (\opts -> opts { optRsrcCpVerbose = False } ))
      ("Don't be chatty when copying the resources directory. Useful when there are a LOT of resources."
         ++ (defaultText . not . optRsrcCpVerbose $ defaultOptions))
   -- , Option ['v'] ["version"]
   --    (NoArg (\opts -> opts { optVersion = True } ))
   --    (printf "Include version in installation path, meaning: %s/PROJECT-VERSION %s"
   --       (optPrefix defaultOptions) (defaultText . optVersion $ defaultOptions))
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


{-
   Recursive file copying code

   It was desireable to have a standalone recursive file copy in
   this script for maximum cross-platform compatibility and to
   avoid Haskell library dependencies.

   Many thanks to [abuzittin gillifirca](https://codereview.stackexchange.com/users/20251/abuzittin-gillifirca) for the StackOverflow post [Copying files in Haskell](https://codereview.stackexchange.com/questions/68908/copying-files-in-haskell) where the following code was lifted.
-}

copyTree :: Bool -> FilePath -> FilePath -> IO ()
copyTree chatty s t = do
    createDirectoryIfMissing True t
    subItems <- getSubitems s
    mapM_ (copyItem chatty s t) subItems


getSubitems :: FilePath -> IO [(Bool, FilePath)]
getSubitems path = getSubitems' ""
  where
    getChildren path' =  (\\ [".", ".."]) <$> getDirectoryContents path'

    getSubitems' relPath = do
        let absPath = path </> relPath
        isDir <- doesDirectoryExist absPath
        children <- if isDir then getChildren absPath else return []
        let relChildren = [relPath </> p | p <- children]
        ((isDir, relPath) :) . concat <$> mapM getSubitems' relChildren


copyItem :: Bool -> FilePath -> FilePath -> (Bool, FilePath) -> IO ()
copyItem chatty baseSourcePath baseTargetPath (isDir, relativePath) = do
    let sourcePath = baseSourcePath </> relativePath
    let targetPath = baseTargetPath </> relativePath

    when chatty $
       putStrLn $ "Copying " ++ sourcePath ++ " to " ++ targetPath

    if isDir
      then createDirectoryIfMissing False targetPath
      else copyFile sourcePath targetPath