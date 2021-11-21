{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HSInstall.Except
  ( HSInstallException (..)
  , justDoIt
  , withExceptionHandling

  -- Re-exported
  , throwM
  )
  where

import Control.Exception.Safe
  ( Exception, Handler (..), Typeable, catches, catchIO, throwM )
import Control.Monad.Catch ( MonadCatch )
import GHC.IO.Exception ( IOException )
import System.Exit ( die )


data HSInstallException
  = NoCabalFiles
  deriving Typeable

instance Show HSInstallException where
  show NoCabalFiles = "no cabal files were found in .  We tried to run `hpack` and/or `stack query` and still don't see a cabal file. Is this directory really a Haskell project?"

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


-- Just try to perform the action and eat any IO exception. We want this to
-- happen because we'll be using this to try several commands and don't expect
-- all of them to succeed.
justDoIt :: MonadCatch m => m () -> m ()
justDoIt a = catchIO a $ const (pure ())
