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
import GHC.IO.Exception ( IOException )
import System.Exit ( die )


data HSInstallException
  = NoCabalFiles
  deriving Typeable

instance Show HSInstallException where
  show NoCabalFiles = "no cabal files were found in .  If this is a buildable project directory that uses hpack, try issuing any stack command like `stack query` or `stack clean` to generate the cabal file from the package.yaml"

instance Exception HSInstallException


withExceptionHandling :: IO a -> IO a
withExceptionHandling = flip catches exceptionHandlers


exceptionHandlers :: [Handler IO a]
exceptionHandlers =
  [ Handler (\(err :: IOException) -> explainError . show $ err)
  , Handler (\(err :: HSInstallException) -> explainError $ show err)
  ]


explainError :: String -> IO a
explainError = die . ("Could not continue because: " ++)
