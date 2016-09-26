module HSInstall
   ( getRsrcDir
   )
   where

import Control.Monad ( liftM2, mplus )
import System.Directory ( doesDirectoryExist )
import System.Environment ( getExecutablePath )
import System.FilePath ( (</>), takeDirectory, takeFileName )


getRsrcDir :: IO FilePath -> IO FilePath
getRsrcDir cabalDataDir =
   maybe (fail "Unable to find resources directory")
      return =<< searchResult

   where
      searchResult :: IO (Maybe FilePath)
      searchResult = foldl (liftM2 mplus) (return Nothing)
         $ (map (>>= mbExists) [ mkRsrcPathFHS cabalDataDir, mkRsrcPathBundle ])

      mbExists :: FilePath -> IO (Maybe FilePath)
      mbExists p = do
         exists <- doesDirectoryExist p
         return $ if exists then Just p else Nothing


mkRsrcPathFHS :: IO FilePath -> IO FilePath
mkRsrcPathFHS cabalDataDir = do
   appDir <- takeFileName <$> cabalDataDir
   ( </> "share" </> appDir </> "resources" ) . takeDirectory . takeDirectory
      <$> getExecutablePath


mkRsrcPathBundle :: IO FilePath
mkRsrcPathBundle =
   ( </> "resources" ) . takeDirectory . takeDirectory <$> getExecutablePath
