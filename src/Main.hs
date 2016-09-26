module Main where

import HSInstall ( getRsrcPath )

import Paths_hsinstall ( getDataDir )


main :: IO ()
main = do
   putStrLn "Running..."

   fooPath <- getRsrcPath getDataDir "foo"
   putStrLn $ "foo resource file path: " ++ fooPath

   readFile fooPath >>= putStr

   barPath <- getRsrcPath getDataDir "bar"
   putStrLn $ "bar resource file path: " ++ barPath

   readFile barPath >>= putStr
