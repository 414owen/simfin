{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SimFin.Internal
  ( SimFinContext(..)
  , apiKeyEnvVariable
  , createDefaultContext
  , baseRequest
  , makeRequest
  , performGenericRequest
  ) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types (parse)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8 as BSU
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status
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

performGenericRequest
  :: ( MonadIO m
     , FromJSON a
     , FromJSON e
     )
  => (LBS.ByteString -> String -> e)
  -> (Value -> String -> e)
  -> SimFinContext
  -> ByteString
  -> [QueryParam]
  -> m (Either e a)
performGenericRequest mkDecodeErr mkParseErr SimFinContext{..} path query = do
  let req = makeRequest simFinApiKey path query
  res <- liftIO $ httpLbs req simFinManager
  let body = responseBody res
  -- Try to parse body into generic JSON
  pure $ case eitherDecode $ responseBody res of
    Left err -> Left $ mkDecodeErr body err
    Right value -> case statusCode $ responseStatus res of
      200 -> case parse parseJSON value of
        Error err -> Left $ mkParseErr value err
        Success a -> Right a
      _ -> case parse parseJSON value of
        Error err -> Left $ mkParseErr value err
        Success a -> Left a
