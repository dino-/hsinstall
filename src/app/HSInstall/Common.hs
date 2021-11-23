{-# LANGUAGE DeriveGeneric #-}

module HSInstall.Common
  ( TmplDir (..)
  , dumpStockIcon
  , tmplDir
  )
  where

import Control.Newtype.Generics
import Data.Maybe ( fromMaybe )
import GHC.Generics
import HSInstall.Paths ( getShareDir )
import Paths_hsinstall ( getDataDir )
import System.Directory ( copyFile )
import System.FilePath ( (</>), (<.>) )


newtype TmplDir = TmplDir FilePath
  deriving Generic

instance Newtype TmplDir

tmplDir :: TmplDir
tmplDir = pack $ "." </> "hsinstall"


dumpStockIcon :: Maybe FilePath -> IO ()
dumpStockIcon mbDestPath = do
  shareDir <- getShareDir getDataDir
  let iconFilename = "unix-terminal" <.> "svg"
  let iconSourcePath = shareDir </> "resources" </> iconFilename
  let destPath = fromMaybe iconFilename mbDestPath
  copyFile iconSourcePath destPath
