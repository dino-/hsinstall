module HSInstall
   ( getRsrcPath )
   where

import Control.Monad ( liftM2, mplus )
import System.Directory ( doesFileExist )
import System.Environment ( getExecutablePath )
import System.FilePath ( (</>), takeDirectory, takeFileName )


getRsrcPath :: IO FilePath -> FilePath -> IO FilePath
getRsrcPath cabalDataDir rel = maybe (fail ("Unable to find resource at relative path: " ++ rel)) return =<< searchResult
   where
      searchResult :: IO (Maybe FilePath)
      searchResult = foldl (liftM2 mplus) (return Nothing) $ (map (>>= mbExists) potentialPaths)

      potentialPaths :: [IO FilePath]
      potentialPaths = map ($ rel) [ mkRsrcPathFHS cabalDataDir, mkRsrcPathBundle ]

      mbExists :: FilePath -> IO (Maybe FilePath)
      mbExists p = do
         exists <- doesFileExist p
         return $ if exists then Just p else Nothing


mkRsrcPathFHS :: IO FilePath -> FilePath -> IO FilePath
mkRsrcPathFHS cabalDataDir rel = do
   appDir <- takeFileName <$> cabalDataDir
   ( </> "share" </> appDir </> "resources" </> rel ) . takeDirectory . takeDirectory <$> getExecutablePath


mkRsrcPathBundle :: FilePath -> IO FilePath
mkRsrcPathBundle rel =
   ( </> "resources" </> rel ) . takeDirectory . takeDirectory <$> getExecutablePath
