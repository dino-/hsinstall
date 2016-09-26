module Main where

import System.Environment ( getExecutablePath )
import System.FilePath ( (</>), takeDirectory )

import Paths_hsinstall ( getDataDir )


main :: IO ()
main = do
   putStrLn "Running..."

   putStrLn =<< ("env executable path: " ++) <$> getExecutablePath
   putStrLn =<< ("cabal data dir: " ++) <$> getDataDir

   rsrcPath <- ( </> "resources" ) . takeDirectory . takeDirectory <$> getExecutablePath
   putStrLn $ "resources path: " ++ rsrcPath

   ( readFile $ rsrcPath </> "foo" ) >>= putStr
