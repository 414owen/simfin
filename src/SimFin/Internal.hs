{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module SimFin.Internal
  ( SimFinContext(..)
  , apiKeyEnvVariable
  , createDefaultContext
  , baseRequest
  , makeRequest
  , performRequest
  ) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as BSU
import Data.Maybe (fromJust)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Environment (lookupEnv)

import SimFin.Util

data SimFinContext = SimFinContext
  { simFinApiKey :: ByteString
  , simFinManager :: Manager
  }

apiKeyEnvVariable :: String
apiKeyEnvVariable = "SIM_FIN_API_KEY"

createDefaultContext :: (MonadFail m, MonadIO m) => m SimFinContext
createDefaultContext = do
  manager <- newTlsManager
  apiKeyOpt <- liftIO $ lookupEnv apiKeyEnvVariable
  case apiKeyOpt of
    Nothing -> fail $ "Couldn't find environment variable '" <> apiKeyEnvVariable <> "'"
    Just apiKey -> pure $ SimFinContext (BSU.fromString apiKey) manager

baseRequest :: Request
baseRequest = defaultRequest
  { host = "simfin.com"
  , port = 443
  , secure = True
  , requestHeaders =
    [ ("Accept", "application/json")
    ]
  }

basePath :: ByteString
basePath = "/api/v2/"

makeRequest :: ByteString -> ByteString -> [QueryParam] -> Request
makeRequest apiKey path query =
  setQueryString (("api-key", Just apiKey) : query)
  $ baseRequest { path = basePath <> path }

performRequest
  :: ( MonadIO m
     , FromJSON a
     )
  => SimFinContext
  -> ByteString
  -> [QueryParam]
  -> m a
performRequest SimFinContext{..} path query = do
  let req = makeRequest simFinApiKey path query
  res <- liftIO $ httpLbs req simFinManager
  pure $ fromJust $ decode $ responseBody res
