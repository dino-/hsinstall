{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HSInstall.Except
  ( HSInstallException (..)
  , withExceptionHandling

  -- Re-exported
  , throwM
  )
  where

import Control.Exception.Safe
  ( Exception, Handler (..), Typeable, catches, throwM )
import Fmt ( (+|), (|+), (+||), (||+) )
import GHC.IO.Exception ( IOException (ioe_filename, ioe_type) )
import System.Exit ( die )


data HSInstallException
  = NoCabalFiles
  | OneExePerAppImage
  deriving Typeable

instance Show HSInstallException where
  show NoCabalFiles = "no cabal files were found in ."
  show OneExePerAppImage = "one executable must be specified to build an AppImage"

instance Exception HSInstallException


withExceptionHandling :: IO a -> IO a
withExceptionHandling = flip catches exceptionHandlers


exceptionHandlers :: [Handler IO a]
exceptionHandlers =
  [ Handler (\(err :: IOException) -> explainError $
      ""+||ioe_type err||+""+|maybe "" (": " ++) (ioe_filename err)|+"")
  , Handler (\(err :: HSInstallException) -> explainError $ show err)
  ]


explainError :: String -> IO a
explainError = die . ("Could not continue because: " ++)
