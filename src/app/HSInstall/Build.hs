module HSInstall.Build
  ( BuildTool
  , clean
  , determineBuildTool
  , installBinaries
  , makeCabal
  )
  where

import System.Directory ( getDirectoryContents )
import System.Process ( callProcess )

import HSInstall.Common ( ExeFile (..) )
import HSInstall.Except ( justDoIt )
import HSInstall.Opts ( BuildMode (AppImageExe, Project) )


data BuildTool = Cabal | Stack

instance Show BuildTool where
  show Cabal = "cabal"
  show Stack = "stack"


determineBuildTool :: IO BuildTool
determineBuildTool = do
  dirContents <- getDirectoryContents "."
  pure $ if "stack.yaml" `elem` dirContents
    then Stack
    else Cabal


clean :: BuildTool -> IO ()
clean Cabal = callProcess "cabal" ["v2-clean"]
clean Stack = callProcess "stack" ["clean"]


modeToStackArg :: BuildMode -> String
modeToStackArg (AppImageExe (ExeFile exeFp)) = ':' : exeFp
modeToStackArg Project                       = ""


installBinaries :: BuildTool -> BuildMode -> FilePath -> IO ()

installBinaries Cabal _ binDir =
  callProcess "cabal"
    [ "v2-install"
    , "--install-method=copy"
    , "--overwrite-policy=always"
    , "--installdir=" ++ binDir
    ]

installBinaries Stack mode binDir =
  callProcess "stack"
    [ "install"
    , modeToStackArg mode 
    , "--local-bin-path=" ++ binDir
    ]


makeCabal :: BuildTool -> IO ()
makeCabal Cabal = justDoIt $ callProcess "hpack" []
makeCabal Stack = justDoIt $ callProcess "stack" ["query"]
 
