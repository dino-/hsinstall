module Main where

import HSInstall ( getRsrcDir )
import System.FilePath ( (</>) )

import Paths_hsinstall ( getDataDir )


main :: IO ()
main = do
   putStrLn "Running..."

   fooPath <- (</> "foo") <$> getRsrcDir getDataDir
   putStrLn $ "foo resource file path: " ++ fooPath

   readFile fooPath >>= putStr
