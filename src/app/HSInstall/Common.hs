module HSInstall.Common
  ( dumpStockIcon
  , tmplDir
  )
  where

import Data.Maybe ( fromMaybe )
import HSInstall.Paths ( getShareDir )
import Paths_hsinstall ( getDataDir )
import System.Directory ( copyFile )
import System.FilePath ( FilePath, (</>), (<.>) )


tmplDir :: FilePath
tmplDir = "." </> "hsinstall"


dumpStockIcon :: Maybe FilePath -> IO ()
dumpStockIcon mbDestPath = do
  shareDir <- getShareDir getDataDir
  let iconFilename = "unix-terminal" <.> "svg"
  let iconSourcePath = shareDir </> "resources" </> iconFilename
  let destPath = fromMaybe iconFilename mbDestPath
  copyFile iconSourcePath destPath
