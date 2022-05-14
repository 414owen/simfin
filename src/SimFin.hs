{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module SimFin
  ( SimFinContext(..)
  , createDefaultContext
  , listCompanies
  ) where

import Control.Category ((>>>))
import Control.Monad (forM)
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString as BS
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Data.String
import Data.Text (Text)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header (RequestHeaders)
import System.Environment (lookupEnv)

type SimFinId = Int

type QueryParam = (ByteString, Maybe ByteString)

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

makeRequest :: SimFinContext -> ByteString -> [QueryParam] -> Request
makeRequest SimFinContext{simFinApiKey} path query =
  baseRequest { path = basePath <> path }
  & setQueryString (("api-key", Just simFinApiKey) : query)

data CompanyListingRow
  = CompanyListingRow
  { simFinId :: Int
  , ticker :: Text
  } deriving Show

type CompanyListing = [CompanyListingRow]

newtype CompanyListingColumnar = CompanyListingColumnar { unColumns :: CompanyListing }

instance FromJSON CompanyListingRow where
  parseJSON (Object v) = CompanyListingRow
    <$> v .: "SimFinId"
    <*> v .: "Ticker"

columnsToRows :: Value -> Parser [Value]
columnsToRows (Object root) = do
  cols :: [Key] <- fmap K.fromText <$> root .: "columns"
  rows :: [[Value]] <- root .: "data"
  pure $ Object . KM.fromList . zip cols <$> rows

instance FromJSON CompanyListingColumnar where
  parseJSON o = CompanyListingColumnar <$> (traverse parseJSON =<< columnsToRows o)

listCompanies :: (MonadThrow m, MonadIO m) => SimFinContext -> m CompanyListing
listCompanies ctx@SimFinContext{simFinManager} = do
  let req = makeRequest ctx "companies/list" []
  res <- liftIO $ httpLbs req simFinManager
  pure $ fromJust $ unColumns <$> decode (responseBody res)

test :: IO ()
test = do
  ctx <- createDefaultContext
  print =<< listCompanies ctx
  pure ()
