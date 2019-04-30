{- |
  Deployed file location support library for software deployed with the
  hsinstall utility. Or, really, anything that roughly follows an FHS-like
  deployment structure.
-}
module HSInstall.Paths
   ( getShareDir )
   where

import System.Directory ( doesDirectoryExist )
import System.Environment ( getExecutablePath )
import System.FilePath ( (</>), takeDirectory, takeFileName )


{- |
  Get a path to the share directory relative to the binary location. The
  argument passed here is expected to be the output of @getDataDir@ generated
  by Cabal at compile time in the @Paths_PROJECTNAME@ module.

  Usage:

  @
    import HSInstall.Resources ( getShareDir )
    import Paths_PROJECTNAME ( getDataDir )

    shareDir <- getShareDir getDataDir
  @

  If your binary is at @\/foo\/bar\/usr\/bin\/BINARY@, this library will generate
  this path: @\/foo\/bar\/usr\/share\/PROJECTNAME@
-}
getShareDir :: IO FilePath -> IO FilePath
getShareDir cabalDataDir = do
  appDir <- stripVersion . takeFileName <$> cabalDataDir
  sharePath <- ( </> "share" </> appDir )
    . takeDirectory . takeDirectory <$> getExecutablePath

  sharePathExists <- doesDirectoryExist sharePath
  if sharePathExists
    then return sharePath
    else fail $ "Share directory " ++ sharePath ++ " does not exist"


stripVersion :: String -> String
stripVersion = reverse . tail . dropWhile (/= '-') . reverse
