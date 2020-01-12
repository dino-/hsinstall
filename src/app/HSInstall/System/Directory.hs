{-# LANGUAGE OverloadedStrings #-}

module HSInstall.System.Directory
  ( copyTree
  )
  where

import Control.Monad ( when )
import Data.List ( (\\) )
import System.Directory ( copyFile, createDirectoryIfMissing,
  doesDirectoryExist, getDirectoryContents )
import System.FilePath ( (</>) )
import Text.Printf ( printf )


{-
  Recursive file copying code

  It proved difficult to find a stock recursive file copy utility that
  preserved the executable bits on things.

  chatty in this context means describe the file copies on stdout.

  Many thanks to [abuzittin gillifirca](https://codereview.stackexchange.com/users/20251/abuzittin-gillifirca)
  for the StackOverflow post [Copying files in Haskell](https://codereview.stackexchange.com/questions/68908/copying-files-in-haskell)
  from which the following code was lifted.
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
    printf "Copying %s to %s\n" sourcePath targetPath

  if isDir
    then createDirectoryIfMissing False targetPath
    else copyFile sourcePath targetPath
