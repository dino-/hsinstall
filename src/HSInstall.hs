module HSInstall
   ( getRsrcDir )
   where

import Control.Monad ( liftM2, mplus )
import System.Directory ( doesDirectoryExist )
import System.Environment ( getExecutablePath )
import System.FilePath ( (</>), takeDirectory, takeFileName )


{- |
   Get the path to the resources, relative to where the binary was
   installed and executed from

   Usage:

      import HSInstall ( getRsrcDir )
      import Paths_your_project ( getDataDir )

      resourcesDir <- getRsrcDir getDataDir
-}
getRsrcDir :: IO FilePath -> IO FilePath
getRsrcDir cabalDataDir =
   maybe (fail "Unable to find resources directory")
      return =<< searchResult

   where
      searchResult :: IO (Maybe FilePath)
      searchResult = foldl (liftM2 mplus) (return Nothing) $ map (>>= mbExists)
         [ mkRsrcPathFHS cabalDataDir
         , mkRsrcPathFHSNoVer cabalDataDir
         , mkRsrcPathBundle
         ]

      mbExists :: FilePath -> IO (Maybe FilePath)
      mbExists p = do
         exists <- doesDirectoryExist p
         return $ if exists then Just p else Nothing


mkRsrcPathFHS :: IO FilePath -> IO FilePath
mkRsrcPathFHS cabalDataDir = do
   appDir <- takeFileName <$> cabalDataDir
   ( </> "share" </> appDir </> "resources" ) . takeDirectory . takeDirectory
      <$> getExecutablePath


mkRsrcPathFHSNoVer :: IO FilePath -> IO FilePath
mkRsrcPathFHSNoVer cabalDataDir = do
   appDir <- takeFileName <$> cabalDataDir
   ( </> "share" </> (removeVersion appDir) </> "resources" ) . takeDirectory . takeDirectory
      <$> getExecutablePath

   where
      removeVersion = init . reverse . dropWhile (/= '-') . reverse


mkRsrcPathBundle :: IO FilePath
mkRsrcPathBundle =
   ( </> "resources" ) . takeDirectory . takeDirectory <$> getExecutablePath
