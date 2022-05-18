{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module SimFin
  ( SimFinContext(..)
  , Industry(..)
  , FiscalPeriod(..)

  , GeneralBalanceSheetRow(..)
  , BankBalanceSheetRow(..)
  , InsuranceBalanceSheetRow(..)

  , GeneralProfitAndLossRow(..)
  , BankProfitAndLossRow(..)
  , InsuranceProfitAndLossRow(..)

  , GeneralCashFlowRow(..)
  , BankCashFlowRow(..)
  , InsuranceCashFlowRow(..)

  , DerivedRow(..)

  , PricesRow(..)
  , RatiosRow(..)
  , PricesAndRatiosRow(..)

  , createDefaultContext
  , fetchCompanyList

  , fetchCompanyInfo
  , fetchBalanceSheets
  , fetchProfitsAndLosses
  , fetchCashFlows
  , fetchDerived
  , fetchPrices
  , fetchPricesAndRatios
  ) where

import Control.Arrow
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.UTF8 as BSU
import Data.Function ((&))
import Data.List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Calendar (Day)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Environment (lookupEnv)

import SimFin.Types.BalanceSheet
import SimFin.Types.CompanyInfo
import SimFin.Types.CashFlow
import SimFin.Types.Derived
import SimFin.Types.Industry
import SimFin.Types.Prices
import SimFin.Types.ProfitAndLoss
import SimFin.Types.Ratios
import SimFin.Util

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

debugRequest
  :: ( MonadIO m
     , FromJSON a
     )
  => SimFinContext
  -> ByteString
  -> [QueryParam]
  -> m a
debugRequest SimFinContext{..} path query = do
  let req = makeRequest simFinApiKey path query
  res <- liftIO $ httpLbs req simFinManager
  case eitherDecode $ responseBody res of
    Left s -> do
      liftIO $ putStrLn s
      liftIO $ putStrLn $ show res
      undefined
    Right a -> pure a

data FiscalPeriod = Q1 | Q2 | Q3 | Q4 | H1 | H2 | FullYear | FirstNineMonths | SixMonths
  deriving (Eq, Show)

fiscalPeriodParam :: FiscalPeriod -> ByteString
fiscalPeriodParam a = case a of
  Q1 -> "q1"
  Q2 -> "q2"
  Q3 -> "q3"
  Q4 -> "q4"
  H1 -> "h1"
  H2 -> "h2"
  FullYear-> "fy"
  FirstNineMonths -> "9m"
  SixMonths -> "6m"

instance FromJSON FiscalPeriod where
  parseJSON = withText "FiscalPeriod" $ \t -> case T.toLower t of
    "q1" -> pure Q1
    "q2" -> pure Q2
    "q3" -> pure Q3
    "q4" -> pure Q4
    "h1" -> pure H1
    "h2" -> pure H2
    "fy" -> pure FullYear
    "9m" -> pure FirstNineMonths
    "6m" -> pure SixMonths
    _ -> fail "Invalid fiscal year string"

data StockRef = SimFinId Int | Ticker Text
  deriving Show

separateStockRefs :: Foldable t => t StockRef -> ([Int], [Text])
separateStockRefs = foldl' f ([], [])
  where
    f :: ([Int], [Text]) -> StockRef -> ([Int], [Text])
    f acc (SimFinId n) = first (n:) acc
    f acc (Ticker t) = second (t:) acc

toStockRefQueryParams :: NonEmpty StockRef -> [(ByteString, Maybe ByteString)]
toStockRefQueryParams refs =
  let
    (ids, tickers) = separateStockRefs refs
    tickerParam = toTextCommaQueryParam "ticker" tickers
    idParam = toShownCommaQueryParam "id" ids
  in tickerParam <> idParam

------
-- List companies
------

data CompanyListingRow
  = CompanyListingRow
  { simFinId :: Int
  , ticker :: Text
  } deriving Show

type CompanyListing = [CompanyListingRow]

newtype CompanyListingKeyed = CompanyListingKeyed { unKeyCompanyListing :: CompanyListing }

instance FromJSON CompanyListingRow where
  parseJSON = withObject "CompanyListing" $ \v -> CompanyListingRow
    <$> v .: "SimFinId"
    <*> v .: "Ticker"

instance FromJSON CompanyListingKeyed where
  parseJSON o = CompanyListingKeyed <$> (traverse parseJSON =<< createKeyedRows o)

fetchCompanyList :: (MonadThrow m, MonadIO m) => SimFinContext -> m CompanyListing
fetchCompanyList ctx =
  unKeyCompanyListing <$> performRequest ctx "companies/list" []

------
-- General Company Info
------

fetchCompanyInfo
  :: (MonadThrow m, MonadIO m)
  => SimFinContext
  -> NonEmpty StockRef
  -> m [CompanyInfoRow]
fetchCompanyInfo ctx refs =
  performRequest ctx "companies/general" (toStockRefQueryParams refs)

-- For SimFin+ users
data StatementQuery
  = StatementQuery
  { stockRefs :: NonEmpty StockRef
  , periods :: [FiscalPeriod]
  , years :: [Int]
  , start :: Maybe Day
  , end :: Maybe Day
  , ttm :: Bool
  , asReported :: Bool
  -- TODO we don't model the result of this yet
  , shares :: Bool
  } deriving Show

toCommaQueryParam :: ByteString -> (a -> ByteString) -> [a] -> [(ByteString, Maybe ByteString)]
toCommaQueryParam key f as = case as of
  [] -> []
  _ -> [(key, Just $ BS8.intercalate "," $ f <$> as)]

-- Chars are truncated to 8-bits
toShownCommaQueryParam :: Show a => ByteString -> [a] -> [(ByteString, Maybe ByteString)]
toShownCommaQueryParam key = toCommaQueryParam key (BS8.pack . show)

toTextCommaQueryParam :: ByteString -> [Text] -> [(ByteString, Maybe ByteString)]
toTextCommaQueryParam key = toCommaQueryParam key T.encodeUtf8

toBoolQueryParam :: ByteString -> Bool -> [(ByteString, Maybe ByteString)]
toBoolQueryParam key b = case b of
  False -> []
  True -> [(key, Nothing)]

statementQueryToQueryParams :: StatementQuery -> [(ByteString, Maybe ByteString)]
statementQueryToQueryParams StatementQuery{..} =
  let
    refParams = toStockRefQueryParams stockRefs
    startParam = toShownCommaQueryParam "start "$ maybeToList start
    endParam = toShownCommaQueryParam "end" $ maybeToList end
    periodParam = toCommaQueryParam "period" fiscalPeriodParam periods
    yearParam = toShownCommaQueryParam "fyear" years
    ttmParam = toBoolQueryParam "ttm" ttm
    asReportedParam = toBoolQueryParam "asreported" asReported
    sharesParam = toBoolQueryParam "shares" shares
  in
  mconcat
    [ refParams
    , startParam
    , endParam
    , periodParam
    , yearParam
    , ttmParam
    , asReportedParam
    , sharesParam
    ]

-- For SimFin- users
-- This is a subset of the SimFin+ API
data StatementQueryFree
  = StatementQueryFree
  { stockRef :: StockRef
  , period :: FiscalPeriod
  , year :: Int
  , ttm :: Bool
  }

freeStatementQueryToPaidStatementQuery :: StatementQueryFree -> StatementQuery
freeStatementQueryToPaidStatementQuery StatementQueryFree{..}
  = StatementQuery
  { stockRefs = pure stockRef
  , periods = pure period
  , years = pure year
  , start = Nothing
  , end = Nothing
  , ttm = ttm
  , asReported = False
  , shares = False
  }

statementQueryFreeToQueryParams :: StatementQueryFree -> [(ByteString, Maybe ByteString)]
statementQueryFreeToQueryParams = statementQueryToQueryParams . freeStatementQueryToPaidStatementQuery

------
-- Balance Sheets
------

fetchBalanceSheets
  :: (MonadThrow m, MonadIO m)
  => SimFinContext
  -> StatementQuery
  -> m [IndustryBalanceSheets]
fetchBalanceSheets ctx query = performRequest ctx "companies/statements"
  (("statement", Just "bs") : statementQueryToQueryParams query)

------
-- P&L
------

fetchProfitsAndLosses
  :: (MonadThrow m, MonadIO m)
  => SimFinContext
  -> StatementQuery
  -> m [IndustryProfitsAndLosses]
fetchProfitsAndLosses ctx query = performRequest ctx "companies/statements"
  (("statement", Just "pl") : statementQueryToQueryParams query)

-----
-- Cash Flows
------

fetchCashFlows
  :: (MonadThrow m, MonadIO m)
  => SimFinContext
  -> StatementQuery
  -> m [IndustryCashFlows]
fetchCashFlows ctx query = performRequest ctx "companies/statements"
  (("statement", Just "cf") : statementQueryToQueryParams query)

------
-- Derived
------

fetchDerived
  :: (Read a, RealFrac a, MonadThrow m, MonadIO m)
  => SimFinContext
  -> StatementQuery
  -> m [DerivedRow a]
fetchDerived ctx query =
  mconcat . fmap unDerivedRows <$> performRequest ctx "companies/statements"
    (("statement", Just "derived") : statementQueryToQueryParams query)

------
-- Prices
------

data PricesQuery
  = PricesQuery
  { stockRefs :: NonEmpty StockRef
  , start :: Maybe Day
  , end :: Maybe Day
  , asReported :: Bool
  } deriving Show

priceQueryToQueryParams :: PricesQuery -> [(ByteString, Maybe ByteString)]
priceQueryToQueryParams PricesQuery{..} = 
  let
    refParams = toStockRefQueryParams stockRefs
    startParam = toShownCommaQueryParam "start "$ maybeToList start
    endParam = toShownCommaQueryParam "end" $ maybeToList end
    asReportedParam = toBoolQueryParam "asreported" asReported
  in
  mconcat
    [ refParams
    , startParam
    , endParam
    , asReportedParam
    ]

newtype PricesKeyed a = PricesKeyed { unKeyPrices :: [PricesRow a] }

instance (Read a, RealFrac a) => FromJSON (PricesKeyed a) where
  parseJSON o = PricesKeyed <$> (traverse parseJSON =<< createKeyedRows o)

fetchPrices
  :: (Read a, RealFrac a, MonadThrow m, MonadIO m)
  => SimFinContext
  -> PricesQuery
  -> m [PricesRow a]
fetchPrices ctx query =
  mconcat . fmap unKeyPrices <$> performRequest ctx "companies/prices"
    (priceQueryToQueryParams query)

data PricesAndRatiosRow a
  = PricesAndRatiosRow
  { prices :: PricesRow a
  , ratios :: RatiosRow a
  } deriving Show

instance (Read a, RealFrac a) => FromJSON (PricesAndRatiosRow a) where
  parseJSON v = PricesAndRatiosRow
    <$> parseJSON v
    <*> parseJSON v

newtype PricesAndRatiosKeyed a = PricesAndRatiosKeyed { unKeyPricesAndRatios :: [PricesAndRatiosRow a] }

instance (Read a, RealFrac a) => FromJSON (PricesAndRatiosKeyed a) where
  parseJSON o = PricesAndRatiosKeyed <$> (traverse parseJSON =<< createKeyedRows o)

fetchPricesAndRatios
  :: (Read a, RealFrac a, MonadThrow m, MonadIO m)
  => SimFinContext
  -> PricesQuery
  -> m [PricesAndRatiosRow a]
fetchPricesAndRatios ctx query =
  mconcat . fmap unKeyPricesAndRatios <$> performRequest ctx "companies/prices"
    (("ratios", Nothing) : priceQueryToQueryParams query)

testStatementQuery :: Text -> StatementQuery
testStatementQuery ref = StatementQuery
  { stockRefs = NE.singleton $ Ticker ref
  , periods = [FullYear]
  , years = [2020]
  , start = Nothing
  , end = Nothing
  , ttm = False
  , asReported = False
  , shares = False
  }

testPricesQuery :: Text -> PricesQuery
testPricesQuery ref = PricesQuery
  { stockRefs = NE.singleton $ Ticker ref
  , start = Nothing
  , end = Nothing
  , asReported = False
  }

test :: IO ()
test = do
  ctx <- createDefaultContext
  -- print =<< fetchBalanceSheets ctx (testStatementQuery"AAPL")
  -- print =<< fetchBalanceSheets ctx (testStatementQuery "CB")
  -- print =<< fetchBalanceSheets ctx (testStatementQuery "C")
  -- print =<< fetchProfitsAndLosses ctx (testStatementQuery "GOOG")
  -- print =<< fetchProfitsAndLosses ctx (testStatementQuery "C")
  -- print =<< fetchProfitsAndLosses ctx (testStatementQuery "CB")
  -- print =<< fetchCashFlows ctx (testStatementQuery "GOOG")
  -- print =<< fetchCashFlows ctx (testStatementQuery "C")
  -- print =<< fetchCashFlows ctx (testStatementQuery "CB")
  -- print =<< fetchDerived ctx (testStatementQuery "AAPL")
  -- print =<< fetchPrices ctx (testPricesQuery "AAPL")
  print =<< fetchPricesAndRatios ctx (testPricesQuery "AAPL")
  pure ()

