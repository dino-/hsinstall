{-# LANGUAGE DuplicateRecordFields #-}

module HSInstall.Common
  ( ExeFile (..)
  , Signing (..)
  , TmplDir (..)
  , dumpStockIcon
  , tmplDir
  )
  where

import Data.Maybe ( fromMaybe )
import HSInstall.Paths ( getShareDir )
import Paths_hsinstall ( getDataDir )
import System.Directory ( copyFile )
import System.FilePath ( (</>), (<.>) )


newtype TmplDir = TmplDir { v :: FilePath }

tmplDir :: TmplDir
tmplDir = TmplDir $ "." </> "hsinstall"


newtype ExeFile = ExeFile { v :: FilePath }


data Signing = SigningKeyId String | NoSignature


dumpStockIcon :: Maybe FilePath -> IO ()
dumpStockIcon mbDestPath = do
  shareDir <- getShareDir getDataDir
  let iconFilename = "unix-terminal" <.> "svg"
  let iconSourcePath = shareDir </> "resources" </> iconFilename
  let destPath = fromMaybe iconFilename mbDestPath
  copyFile iconSourcePath destPath
