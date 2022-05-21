{-|
Module      : SimFin.Util
Description : Common utilities.
Copyright   : (c) Owen Shepherd, 2022
License     : MIT
Maintainer  : owen@owen.cafe
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SimFin.Util
  ( createDefaultContext
  , apiKeyEnvVariable
  ) where

import Control.Monad.IO.Class
import qualified Data.ByteString.UTF8 as BSU
import Network.HTTP.Client.TLS
import System.Environment (lookupEnv)

import SimFin.Internal

-- | The environment variable 'createDefaultContext' will try to get your
-- API key from.

apiKeyEnvVariable :: String
apiKeyEnvVariable = "SIM_FIN_API_KEY"

-- | Try to make a new http-client manager, and parse your api key from 
-- 'apiKeyEnvVariable'.

createDefaultContext :: (MonadFail m, MonadIO m) => m SimFinContext
createDefaultContext = do
  manager <- newTlsManager
  apiKeyOpt <- liftIO $ lookupEnv apiKeyEnvVariable
  case apiKeyOpt of
    Nothing -> fail $ "Couldn't find environment variable '" <> apiKeyEnvVariable <> "'"
    Just apiKey -> pure $ SimFinContext (BSU.fromString apiKey) manager

