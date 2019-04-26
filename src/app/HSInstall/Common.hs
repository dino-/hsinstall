module HSInstall.Common
  ( dumpStockIcon
  , stackClean
  )
  where

import Data.Maybe ( fromMaybe )
import HSInstall.Resources ( getRsrcDir )
import Paths_hsinstall ( getDataDir )
import System.Directory ( copyFile )
import System.FilePath ( (</>), (<.>) )
import System.Process ( callProcess )


stackClean :: IO ()
stackClean = callProcess "stack" ["clean"]


dumpStockIcon :: Maybe FilePath -> IO ()
dumpStockIcon mbDestPath = do
  resourcesDir <- getRsrcDir getDataDir
  let iconFilename = "unix-terminal" <.> "svg"
  let iconSourcePath = resourcesDir </> iconFilename
  let destPath = fromMaybe iconFilename mbDestPath
  copyFile iconSourcePath destPath
