module HSInstall.Common
  ( dumpStockIcon
  )
  where

import Data.Maybe ( fromMaybe )
import HSInstall.Paths ( getShareDir )
import Paths_hsinstall ( getDataDir )
import System.Directory ( copyFile )
import System.FilePath ( (</>), (<.>) )


dumpStockIcon :: Maybe FilePath -> IO ()
dumpStockIcon mbDestPath = do
  shareDir <- getShareDir getDataDir
  let iconFilename = "unix-terminal" <.> "svg"
  let iconSourcePath = shareDir </> "resources" </> iconFilename
  let destPath = fromMaybe iconFilename mbDestPath
  copyFile iconSourcePath destPath
