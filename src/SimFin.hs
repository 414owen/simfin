{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module SimFin
  ( SimFinContext(..)
  , createDefaultContext
  , listCompanies
  , companyInformationById
  ) where

import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.UTF8 as BSU
import Data.Function ((&))
import Data.Maybe (fromJust)
import Data.Text (Text)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Environment (lookupEnv)

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

makeRequest :: ByteString -> ByteString -> [QueryParam] -> Request
makeRequest apiKey path query =
  baseRequest { path = basePath <> path }
  & setQueryString (("api-key", Just apiKey) : query)

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

createKeyedRows :: Value -> Parser [Value]
createKeyedRows = withObject "Root" $ \root -> do
  cols :: [Key] <- fmap K.fromText <$> root .: "columns"
  rows :: [[Value]] <- root .: "data"
  pure $ Object . KM.fromList . zip cols <$> rows

createKeyedRow :: Value -> Parser Value
createKeyedRow = withObject "Root" $ \root -> do
  cols :: [Key] <- fmap K.fromText <$> root .: "columns"
  row :: [Value] <- root .: "data"
  pure $ Object $ KM.fromList $ zip cols row

------
-- List companies
------

data CompanyListingRow
  = CompanyListingRow
  { simFinId :: Int
  , ticker :: Text
  } deriving Show

type CompanyListing = [CompanyListingRow]

newtype CompanyListingKeyed = CompanyListingKeyed { unKeyedCompanyListing :: CompanyListing }

instance FromJSON CompanyListingRow where
  parseJSON = withObject "CompanyListing" $ \v -> CompanyListingRow
    <$> v .: "SimFinId"
    <*> v .: "Ticker"

instance FromJSON CompanyListingKeyed where
  parseJSON o = CompanyListingKeyed <$> (traverse parseJSON =<< createKeyedRows o)

listCompanies :: (MonadThrow m, MonadIO m) => SimFinContext -> m CompanyListing
listCompanies ctx =
  unKeyedCompanyListing <$> performRequest ctx "companies/list" []

------
-- General Company Info
------

data CompanyInformation
  = CompanyInformation
  { simFinId :: Int
  , ticker :: Text
  , companyName :: Text
  , industryId :: Int
  , monthFYEnd :: Int
  , numberEmployees :: Int
  , businessSummary :: Text
  } deriving Show

newtype CompanyInfoKeyed = CompanyInfoKeyed { unKeyedCompanyInfo :: CompanyInformation }

instance FromJSON CompanyInformation where
  parseJSON = withObject "CompanyInformation" $ \v -> CompanyInformation
    <$> v .: "SimFinId"
    <*> v .: "Ticker"
    <*> v .: "Company Name"
    <*> v .: "IndustryId"
    <*> v .: "Month FY End"
    <*> v .: "Number Employees"
    <*> v .: "Business Summary"

instance FromJSON CompanyInfoKeyed where
  parseJSON o = CompanyInfoKeyed <$> (parseJSON =<< createKeyedRow o)

companyInformationById :: (MonadThrow m, MonadIO m) => SimFinContext -> [Int] -> m [CompanyInformation]
companyInformationById ctx ids =
  fmap unKeyedCompanyInfo <$> performRequest ctx "companies/general"
    [ ("id", Just $ BS8.intercalate "," $ BS8.pack . show <$> ids) ]
