module HSInstall.Common
  ( stackClean )
  where

import System.Process ( callProcess )


stackClean :: IO ()
stackClean = callProcess "stack" ["clean"]
