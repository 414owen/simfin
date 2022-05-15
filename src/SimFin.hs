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

  , createDefaultContext
  , fetchCompanyList

  , fetchCompanyInformationById
  , fetchCompanyInformationByTicker
  , fetchBalanceSheetsByTicker
  , fetchProfitsAndLossesByTicker
  , fetchCashFlowsByTicker
  , fetchDerivedByTicker
  ) where

import Control.Applicative ((<|>))
import Control.Arrow
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.UTF8 as BSU
import Data.List.NonEmpty (NonEmpty)
import Data.Time.Calendar (Day)
import Data.Function ((&))
import Data.List
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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

{-
debugRequest
  :: ( MonadIO m
     , FromJSON a
     )
  => SimFinContext
  -> ByteString
  -> [QueryParam]
  -> m (Either String a)
debugRequest SimFinContext{..} path query = do
  let req = makeRequest simFinApiKey path query
  res <- liftIO $ httpLbs req simFinManager
  pure $ eitherDecode $ responseBody res
-}

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

data Industry general bank insurance
  = General general
  | Bank bank
  | Insurance insurance
  deriving Show

mapIndustry :: (a -> a') -> (b -> b') -> (c -> c') -> Industry a b c -> Industry a' b' c'
mapIndustry f g h industry = case industry of
  General a -> General $ f a
  Bank a -> Bank $ g a
  Insurance a -> Insurance $ h a

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

newtype CompanyInfoKeyed = CompanyInfoKeyed { unKeyCompanyInfo :: CompanyInformation }

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

fetchCompanyInformationById :: (MonadThrow m, MonadIO m) => SimFinContext -> [Int] -> m [CompanyInformation]
fetchCompanyInformationById ctx ids =
  fmap unKeyCompanyInfo <$> performRequest ctx "companies/general"
    [ ("id", Just $ BS8.intercalate "," $ BS8.pack . show <$> ids) ]

fetchCompanyInformationByTicker :: (MonadThrow m, MonadIO m) => SimFinContext -> [Text] -> m [CompanyInformation]
fetchCompanyInformationByTicker ctx tickers =
  fmap unKeyCompanyInfo <$> performRequest ctx "companies/general"
    [ ("ticker", Just $ T.encodeUtf8 $ T.intercalate "," tickers) ]

data StockRef = SimFinId Int | Ticker Text
  deriving Show

separateStockRefs :: Foldable t => t StockRef -> ([Int], [Text])
separateStockRefs = foldl' f ([], [])
  where
    f :: ([Int], [Text]) -> StockRef -> ([Int], [Text])
    f acc (SimFinId n) = first (n:) acc
    f acc (Ticker t) = second (t:) acc

data StatementQuery
  = StatementQuery
  { stockRefs :: NonEmpty StockRef
  , periods :: [FiscalPeriod]
  , years :: [Int]
  , start :: Maybe Day
  , end :: Maybe Day
  , ttm :: Bool
  , asReported :: Bool
  , shares :: Bool
  } deriving Show

statementQueryToQueryParams :: StatementQuery -> [(ByteString, Maybe ByteString)]
statementQueryToQueryParams StatementQuery{..} =
  let
    (ids, tickers) = separateStockRefs stockRefs
    idParam = if ids == [] then [] else [("id", Just $ BS8.intercalate "," $ BS8.pack . show <$> ids)]
    tickerParam = if tickers == [] then [] else [("id", Just $ BS8.intercalate "," $ T.encodeUtf8 <$> tickers)]
    startParam = maybe [] (\a -> [("start", Just $ BS8.pack $ show a)]) start
    endParam = maybe [] (\a -> [("start", Just $ BS8.pack $ show a)]) end
    periodParam = if periods == [] then [] else [("period", Just $ BS8.intercalate "," $ fiscalPeriodParam <$> periods)]
    yearParam = if years == [] then [] else [("fyear", Just $ BS8.intercalate "," $ BS8.pack . show <$> years)]
    ttmParam = if ttm then [("ttm", Nothing)] else []
    asReportedParam = if asReported then [("asreported", Nothing)] else []
    sharesParam = if shares then [("shares", Nothing)] else []
  in
  mconcat
    [ tickerParam
    , idParam
    , startParam
    , endParam
    , periodParam
    , yearParam
    , ttmParam
    , asReportedParam
    , sharesParam
    ]


------
-- Balance Sheets
------

data GeneralBalanceSheetRow
  = GeneralBalanceSheetRow
  { simFinId :: Int
  , ticker :: Text
  , fiscalPeriod :: String
  , fiscalYear :: Int
  , reportDate :: Day
  , publishDate :: Day
  , restatedDate :: Day
  , source :: Text
  , tTM :: Bool
  , valueCheck :: Bool
  , cashCashEquivalentsAndShortTermInvestments :: Maybe Integer
  , cashAndCashEquivalents :: Maybe Integer
  , shortTermInvestments :: Maybe Integer
  , accountsAndNotesReceivable :: Maybe Integer
  , accountsReceivableNet :: Maybe Integer
  , notesReceivableNet :: Maybe Integer
  , unbilledRevenues :: Maybe Integer
  , inventories :: Maybe Integer
  , rawMaterials :: Maybe Integer
  , workInProcess :: Maybe Integer
  , finishedGoods :: Maybe Integer
  , otherInventory :: Maybe Integer
  , otherShortTermAssets :: Maybe Integer
  , prepaidExpenses :: Maybe Integer
  , derivativeAndHedgingAssetsShortTerm :: Maybe Integer
  , assetsHeldForSale :: Maybe Integer
  , deferredTaxAssetsShortTerm :: Maybe Integer
  , incomeTaxesReceivable :: Maybe Integer
  , discontinuedOperationsShortTerm :: Maybe Integer
  , miscShortTermAssets :: Maybe Integer
  , totalCurrentAssets :: Maybe Integer
  , propertyPlantAndEquipmentNet :: Maybe Integer
  , propertyPlantAndEquipment :: Maybe Integer
  , accumulatedDepreciation :: Maybe Integer
  , longTermInvestmentsAndReceivables :: Maybe Integer
  , longTermInvestments :: Maybe Integer
  , longTermMarketableSecurities :: Maybe Integer
  , longTermReceivables :: Maybe Integer
  , otherLongTermAssets :: Maybe Integer
  , intangibleAssets :: Maybe Integer
  , goodwill :: Maybe Integer
  , otherIntangibleAssets :: Maybe Integer
  , prepaidExpense :: Maybe Integer
  , deferredTaxAssetsLongTerm :: Maybe Integer
  , derivativeAndHedgingAssetsLongTerm :: Maybe Integer
  , prepaidPensionCosts :: Maybe Integer
  , discontinuedOperationsLongTerm :: Maybe Integer
  , investmentsinAffiliates :: Maybe Integer
  , miscLongTermAssets :: Maybe Integer
  , totalNoncurrentAssets :: Maybe Integer
  , totalAssets :: Maybe Integer
  , payablesAndAccruals :: Maybe Integer
  , accountsPayable :: Maybe Integer
  , accruedTaxes :: Maybe Integer
  , interestAndDividendsPayable :: Maybe Integer
  , otherPayablesAndAccruals :: Maybe Integer
  , shortTermDebt :: Maybe Integer
  , shortTermBorrowings :: Maybe Integer
  , shortTermCapitalLeases :: Maybe Integer
  , currentPortionOfLongTermDebt :: Maybe Integer
  , otherShortTermLiabilities :: Maybe Integer
  , deferredRevenueShortTerm :: Maybe Integer
  , liabilitiesfromDerivativesAndHedgingShortTerm :: Maybe Integer
  , deferredTaxLiabilitiesShortTerm :: Maybe Integer
  , liabilitiesfromDiscontinuedOperationsShortTerm :: Maybe Integer
  , miscShortTermLiabilities :: Maybe Integer
  , totalCurrentLiabilities :: Maybe Integer
  , longTermDebt :: Maybe Integer
  , longTermBorrowings :: Maybe Integer
  , longTermCapitalLeases :: Maybe Integer
  , otherLongTermLiabilities :: Maybe Integer
  , accruedLiabilities :: Maybe Integer
  , pensionLiabilities :: Maybe Integer
  , pensions :: Maybe Integer
  , otherPostRetirementBenefits :: Maybe Integer
  , deferredCompensation :: Maybe Integer
  , deferredRevenueLongTerm :: Maybe Integer
  , deferredTaxLiabilitiesLongTerm :: Maybe Integer
  , liabilitiesfromDerivativesAndHedgingLongTerm :: Maybe Integer
  , liabilitiesfromDiscontinuedOperationsLongTerm :: Maybe Integer
  , miscLongTermLiabilities :: Maybe Integer
  , totalNoncurrentLiabilities :: Maybe Integer
  , totalLiabilities :: Maybe Integer
  , preferredEquity :: Maybe Integer
  , shareCapitalAndAdditionalPaidInCapital :: Maybe Integer
  , commonStock :: Maybe Integer
  , additionalPaidinCapital :: Maybe Integer
  , otherShareCapital :: Maybe Integer
  , treasuryStock :: Maybe Integer
  , retainedEarnings :: Maybe Integer
  , otherEquity :: Maybe Integer
  , equityBeforeMinorityInterest :: Maybe Integer
  , minorityInterest :: Maybe Integer
  , totalEquity :: Maybe Integer
  , totalLiabilitiesAndEquity :: Maybe Integer
  } deriving Show

data BankBalanceSheetRow =
  BankBalanceSheetRow
  { simFinId :: Int
  , ticker :: Text
  , fiscalPeriod :: String
  , fiscalYear :: Int
  , reportDate :: Day
  , publishDate :: Day
  , restatedDate :: Day
  , source :: Text
  , tTM :: Bool
  , valueCheck :: Bool
  , cashCashEquivalentsAndShortTermInvestments :: Maybe Integer
  , interbankAssets :: Maybe Integer
  , fedFundsSoldAndRepos :: Maybe Integer
  , otherInterbankAssets :: Maybe Integer
  , shortAndLongTermInvestments :: Maybe Integer
  , tradingSecurities :: Maybe Integer
  , investmentSecuritiesAvailableforSale :: Maybe Integer
  , investmentSecuritiesHeldtoMaturity :: Maybe Integer
  , realEstateInvestments :: Maybe Integer
  , otherInvestments :: Maybe Integer
  , accountsAndNotesReceivable :: Maybe Integer
  , netLoans :: Maybe Integer
  , reserveforLoanLosses :: Maybe Integer
  , totalLoans :: Maybe Integer
  , totalCommercialLoans :: Maybe Integer
  , commercialRealEstateLoans :: Maybe Integer
  , otherCommercialLoans :: Maybe Integer
  , totalConsumerLoans :: Maybe Integer
  , creditCardLoans :: Maybe Integer
  , homeEquityLoans :: Maybe Integer
  , familyResidentialLoans :: Maybe Integer
  , autoLoans :: Maybe Integer
  , studentLoans :: Maybe Integer
  , otherConsumerLoans :: Maybe Integer
  , otherLoans :: Maybe Integer
  , netFixedAssets :: Maybe Integer
  , propertyPlantAndEquipmentNet :: Maybe Integer
  , operatingLeaseAssets :: Maybe Integer
  , otherFixedAssets :: Maybe Integer
  , intangibleAssets :: Maybe Integer
  , goodwill :: Maybe Integer
  , otherIntangibleAssets :: Maybe Integer
  , investmentsInAssociates :: Maybe Integer
  , deferredTaxAssetsShortTerm :: Maybe Integer
  , derivativesAndHedgingAssets :: Maybe Integer
  , discontinuedOperationsAssets :: Maybe Integer
  , customerAcceptancesAndLiabilities :: Maybe Integer
  , otherAssets :: Maybe Integer
  , totalAssets :: Maybe Integer
  , totalDeposits :: Maybe Integer
  , demandDeposits :: Maybe Integer
  , interestBearingDeposits :: Maybe Integer
  , savingDeposits :: Maybe Integer
  , timeDeposits :: Maybe Integer
  , otherDeposits :: Maybe Integer
  , shortTermDebt :: Maybe Integer
  , securitiesSoldUnderRepo :: Maybe Integer
  , tradingAccountLiabilities :: Maybe Integer
  , shortTermCapitalLeases :: Maybe Integer
  , currentPortionofLongTermDebt :: Maybe Integer
  , shortTermBorrowings :: Maybe Integer
  , payablesBrokerDealers :: Maybe Integer
  , longTermDebt :: Maybe Integer
  , longTermCapitalLeases :: Maybe Integer
  , longTermBorrowings :: Maybe Integer
  , pensionLiabilities :: Maybe Integer
  , pensions :: Maybe Integer
  , otherPostRetirementBenefits :: Maybe Integer
  , deferredTaxLiabilitiesShortTerm :: Maybe Integer
  , derivativesAndHedgingLiabilities :: Maybe Integer
  , discontinuedOperationsLiabilities :: Maybe Integer
  , otherLiabilities :: Maybe Integer
  , totalLiabilities :: Maybe Integer
  , preferredEquity :: Maybe Integer
  , shareCapitalAndAdditionalPaidInCapital :: Maybe Integer
  , commonStock :: Maybe Integer
  , additionalPaidInCapital :: Maybe Integer
  , otherShareCapital :: Maybe Integer
  , treasuryStock :: Maybe Integer
  , retainedEarnings :: Maybe Integer
  , otherEquity :: Maybe Integer
  , equityBeforeMinorityInterest :: Maybe Integer
  , minorityInterest :: Maybe Integer
  , totalEquity :: Maybe Integer
  , totalLiabilitiesAndEquity :: Maybe Integer
  } deriving Show

data InsuranceBalanceSheetRow =
  InsuranceBalanceSheetRow
  { simFinId :: Int
  , ticker :: Text
  , fiscalPeriod :: String
  , fiscalYear :: Int
  , reportDate :: Day
  , publishDate :: Day
  , restatedDate :: Day
  , source :: Text
  , tTM :: Bool
  , valueCheck :: Bool
  , totalInvestments :: Maybe Integer
  , fixedIncomeTradingAFSAndShortTermInv :: Maybe Integer
  , loansAndMortgages :: Maybe Integer
  , fixedIncomeSecuritiesHTM :: Maybe Integer
  , equitySecurities :: Maybe Integer
  , realEstateInvestments :: Maybe Integer
  , otherInvestments :: Maybe Integer
  , cashCashEquivalentsAndShortTermInvestments :: Maybe Integer
  , accountsAndNotesReceivable :: Maybe Integer
  , propertyPlantAndEquipmentNet :: Maybe Integer
  , deferredPolicyAcquisitionCosts :: Maybe Integer
  , otherAssets :: Maybe Integer
  , totalAssets :: Maybe Integer
  , insuranceReserves :: Maybe Integer
  , reserveForOutstandingClaimsAndLosses :: Maybe Integer
  , premiumReserveUnearned :: Maybe Integer
  , lifePolicyBenefits :: Maybe Integer
  , otherInsuranceReserves :: Maybe Integer
  , shortTermDebt :: Maybe Integer
  , otherShortTermLiabilities :: Maybe Integer
  , longTermDebt :: Maybe Integer
  , pensionLiabilities :: Maybe Integer
  , pensions :: Maybe Integer
  , otherPostRetirementBenefits :: Maybe Integer
  , otherLongTermLiabilities :: Maybe Integer
  , fundsForFutureAppropriations :: Maybe Integer
  , totalLiabilities :: Maybe Integer
  , preferredEquity :: Maybe Integer
  , policyholdersEquity :: Maybe Integer
  , shareCapitalAndAdditionalPaidInCapital :: Maybe Integer
  , commonStock :: Maybe Integer
  , additionalPaidInCapital :: Maybe Integer
  , otherShareCapital :: Maybe Integer
  , treasuryStock :: Maybe Integer
  , retainedEarnings :: Maybe Integer
  , otherEquity :: Maybe Integer
  , equityBeforeMinorityInterest :: Maybe Integer
  , minorityInterest :: Maybe Integer
  , totalEquity :: Maybe Integer
  , totalLiabilitiesAndEquity :: Maybe Integer
  } deriving Show

instance FromJSON GeneralBalanceSheetRow where
  parseJSON = withObject "GeneralBalanceSheetRow" $ \v -> GeneralBalanceSheetRow
    <$> v .: "SimFinId"
    <*> v .: "Ticker"
    <*> v .: "Fiscal Period"
    <*> v .: "Fiscal Year"
    <*> v .: "Report Date"
    <*> v .: "Publish Date"
    <*> v .: "Restated Date"
    <*> v .: "Source"
    <*> v .: "TTM"
    <*> v .: "Value Check"
    <*> v .: "Cash, Cash Equivalents & Short Term Investments"
    <*> v .: "Cash & Cash Equivalents"
    <*> v .: "Short Term Investments"
    <*> v .: "Accounts & Notes Receivable"
    <*> v .: "Accounts Receivable, Net"
    <*> v .: "Notes Receivable, Net"
    <*> v .: "Unbilled Revenues"
    <*> v .: "Inventories"
    <*> v .: "Raw Materials"
    <*> v .: "Work In Process"
    <*> v .: "Finished Goods"
    <*> v .: "Other Inventory"
    <*> v .: "Other Short Term Assets"
    <*> v .: "Prepaid Expenses"
    <*> v .: "Derivative & Hedging Assets (Short Term)"
    <*> v .: "Assets Held-for-Sale"
    <*> v .: "Deferred Tax Assets (Short Term)"
    <*> v .: "Income Taxes Receivable"
    <*> v .: "Discontinued Operations (Short Term)"
    <*> v .: "Misc. Short Term Assets"
    <*> v .: "Total Current Assets"
    <*> v .: "Property, Plant & Equipment, Net"
    <*> v .: "Property, Plant & Equipment"
    <*> v .: "Accumulated Depreciation"
    <*> v .: "Long Term Investments & Receivables"
    <*> v .: "Long Term Investments"
    <*> v .: "Long Term Marketable Securities"
    <*> v .: "Long Term Receivables"
    <*> v .: "Other Long Term Assets"
    <*> v .: "Intangible Assets"
    <*> v .: "Goodwill"
    <*> v .: "Other Intangible Assets"
    <*> v .: "Prepaid Expense"
    <*> v .: "Deferred Tax Assets (Long Term)"
    <*> v .: "Derivative & Hedging Assets (Long Term)"
    <*> v .: "Prepaid Pension Costs"
    <*> v .: "Discontinued Operations (Long Term)"
    <*> v .: "Investments in Affiliates"
    <*> v .: "Misc. Long Term Assets"
    <*> v .: "Total Noncurrent Assets"
    <*> v .: "Total Assets"
    <*> v .: "Payables & Accruals"
    <*> v .: "Accounts Payable"
    <*> v .: "Accrued Taxes"
    <*> v .: "Interest & Dividends Payable"
    <*> v .: "Other Payables & Accruals"
    <*> v .: "Short Term Debt"
    <*> v .: "Short Term Borrowings"
    <*> v .: "Short Term Capital Leases"
    <*> v .: "Current Portion of Long Term Debt"
    <*> v .: "Other Short Term Liabilities"
    <*> v .: "Deferred Revenue (Short Term)"
    <*> v .: "Liabilities from Derivatives & Hedging (Short Term)"
    <*> v .: "Deferred Tax Liabilities (Short Term)"
    <*> v .: "Liabilities from Discontinued Operations (Short Term)"
    <*> v .: "Misc. Short Term Liabilities"
    <*> v .: "Total Current Liabilities"
    <*> v .: "Long Term Debt"
    <*> v .: "Long Term Borrowings"
    <*> v .: "Long Term Capital Leases"
    <*> v .: "Other Long Term Liabilities"
    <*> v .: "Accrued Liabilities"
    <*> v .: "Pension Liabilities"
    <*> v .: "Pensions"
    <*> v .: "Other Post-Retirement Benefits"
    <*> v .: "Deferred Compensation"
    <*> v .: "Deferred Revenue (Long Term)"
    <*> v .: "Deferred Tax Liabilities (Long Term)"
    <*> v .: "Liabilities from Derivatives & Hedging (Long Term)"
    <*> v .: "Liabilities from Discontinued Operations (Long Term)"
    <*> v .: "Misc. Long Term Liabilities"
    <*> v .: "Total Noncurrent Liabilities"
    <*> v .: "Total Liabilities"
    <*> v .: "Preferred Equity"
    <*> v .: "Share Capital & Additional Paid-In Capital"
    <*> v .: "Common Stock"
    <*> v .: "Additional Paid in Capital"
    <*> v .: "Other Share Capital"
    <*> v .: "Treasury Stock"
    <*> v .: "Retained Earnings"
    <*> v .: "Other Equity"
    <*> v .: "Equity Before Minority Interest"
    <*> v .: "Minority Interest"
    <*> v .: "Total Equity"
    <*> v .: "Total Liabilities & Equity"

instance FromJSON BankBalanceSheetRow where
  parseJSON = withObject "BankBalanceSheetRow" $ \v -> BankBalanceSheetRow
    <$> v .: "SimFinId"
    <*> v .: "Ticker"
    <*> v .: "Fiscal Period"
    <*> v .: "Fiscal Year"
    <*> v .: "Report Date"
    <*> v .: "Publish Date"
    <*> v .: "Restated Date"
    <*> v .: "Source"
    <*> v .: "TTM"
    <*> v .: "Value Check"
    <*> v .: "Cash, Cash Equivalents & Short Term Investments"
    <*> v .: "Interbank Assets"
    <*> v .: "Fed Funds Sold & Repos"
    <*> v .: "Other Interbank Assets"
    <*> v .: "Short & Long Term Investments"
    <*> v .: "Trading Securities"
    <*> v .: "Investment Securities Available for Sale"
    <*> v .: "Investment Securities Held to Maturity"
    <*> v .: "Real Estate Investments"
    <*> v .: "Other Investments"
    <*> v .: "Accounts & Notes Receivable"
    <*> v .: "Net Loans"
    <*> v .: "Reserve for Loan Losses"
    <*> v .: "Total Loans"
    <*> v .: "Total Commercial Loans"
    <*> v .: "Commercial Real Estate Loans"
    <*> v .: "Other Commercial Loans"
    <*> v .: "Total Consumer Loans"
    <*> v .: "Credit Card Loans"
    <*> v .: "Home Equity Loans"
    <*> v .: "Family Residential Loans"
    <*> v .: "Auto Loans"
    <*> v .: "Student Loans"
    <*> v .: "Other Consumer Loans"
    <*> v .: "Other Loans"
    <*> v .: "Net Fixed Assets"
    <*> v .: "Property, Plant & Equipment, Net"
    <*> v .: "Operating Lease Assets"
    <*> v .: "Other Fixed Assets"
    <*> v .: "Intangible Assets"
    <*> v .: "Goodwill"
    <*> v .: "Other Intangible Assets"
    <*> v .: "Investments in Associates"
    <*> v .: "Deferred Tax Assets (Short Term)"
    <*> v .: "Derivatives & Hedging (Assets)"
    <*> v .: "Discontinued Operations (Assets)"
    <*> v .: "Customer Acceptances & Liabilities"
    <*> v .: "Other Assets"
    <*> v .: "Total Assets"
    <*> v .: "Total Deposits"
    <*> v .: "Demand Deposits"
    <*> v .: "Interest Bearing Deposits"
    <*> v .: "Saving Deposits"
    <*> v .: "Time Deposits"
    <*> v .: "Other Deposits"
    <*> v .: "Short Term Debt"
    <*> v .: "Securities Sold Under Repo"
    <*> v .: "Trading Account Liabilities"
    <*> v .: "Short Term Capital Leases"
    <*> v .: "Current Portion of Long Term Debt"
    <*> v .: "Short Term Borrowings"
    <*> v .: "Payables Broker Dealers"
    <*> v .: "Long Term Debt"
    <*> v .: "Long Term Capital Leases"
    <*> v .: "Long Term Borrowings"
    <*> v .: "Pension Liabilities"
    <*> v .: "Pensions"
    <*> v .: "Other Post-Retirement Benefits"
    <*> v .: "Deferred Tax Liabilities (Short Term)"
    <*> v .: "Derivatives & Hedging (Liabilities)"
    <*> v .: "Discontinued Operations (Liabilities)"
    <*> v .: "Other Liabilities"
    <*> v .: "Total Liabilities"
    <*> v .: "Preferred Equity"
    <*> v .: "Share Capital & Additional Paid-In Capital"
    <*> v .: "Common Stock"
    <*> v .: "Additional Paid in Capital"
    <*> v .: "Other Share Capital"
    <*> v .: "Treasury Stock"
    <*> v .: "Retained Earnings"
    <*> v .: "Other Equity"
    <*> v .: "Equity Before Minority Interest"
    <*> v .: "Minority Interest"
    <*> v .: "Total Equity"
    <*> v .: "Total Liabilities & Equity"

instance FromJSON InsuranceBalanceSheetRow where
  parseJSON = withObject "InsuranceBalanceSheetRow" $ \v -> InsuranceBalanceSheetRow
    <$> v .: "SimFinId"
    <*> v .: "Ticker"
    <*> v .: "Fiscal Period"
    <*> v .: "Fiscal Year"
    <*> v .: "Report Date"
    <*> v .: "Publish Date"
    <*> v .: "Restated Date"
    <*> v .: "Source"
    <*> v .: "TTM"
    <*> v .: "Value Check"
    <*> v .: "Total Investments"
    <*> v .: "Fixed Income-Trading/AFS & Short Term Inv."
    <*> v .: "Loans & Mortgages"
    <*> v .: "Fixed Income Securities HTM"
    <*> v .: "Equity Securities"
    <*> v .: "Real Estate Investments"
    <*> v .: "Other Investments"
    <*> v .: "Cash, Cash Equivalents & Short Term Investments"
    <*> v .: "Accounts & Notes Receivable"
    <*> v .: "Property, Plant & Equipment, Net"
    <*> v .: "Deferred Policy Acquisition Costs"
    <*> v .: "Other Assets"
    <*> v .: "Total Assets"
    <*> v .: "Insurance Reserves"
    <*> v .: "Reserve for Outstanding Claims & Losses"
    <*> v .: "Premium Reserve (Unearned)"
    <*> v .: "Life Policy Benefits"
    <*> v .: "Other Insurance Reserves"
    <*> v .: "Short Term Debt"
    <*> v .: "Other Short Term Liabilities"
    <*> v .: "Long Term Debt"
    <*> v .: "Pension Liabilities"
    <*> v .: "Pensions"
    <*> v .: "Other Post-Retirement Benefits"
    <*> v .: "Other Long Term Liabilities"
    <*> v .: "Funds for Future Appropriations"
    <*> v .: "Total Liabilities"
    <*> v .: "Preferred Equity"
    <*> v .: "Policyholders Equity"
    <*> v .: "Share Capital & Additional Paid-In Capital"
    <*> v .: "Common Stock"
    <*> v .: "Additional Paid in Capital"
    <*> v .: "Other Share Capital"
    <*> v .: "Treasury Stock"
    <*> v .: "Retained Earnings"
    <*> v .: "Other Equity"
    <*> v .: "Equity Before Minority Interest"
    <*> v .: "Minority Interest"
    <*> v .: "Total Equity"
    <*> v .: "Total Liabilities & Equity"

newtype GeneralBalanceSheetsKeyed = GeneralBalanceSheetsKeyed { unKeyGeneralBalanceSheets :: [GeneralBalanceSheetRow] }

instance FromJSON GeneralBalanceSheetsKeyed where
  parseJSON o = GeneralBalanceSheetsKeyed <$> (traverse parseJSON =<< createKeyedRows o)

newtype BankBalanceSheetsKeyed = BankBalanceSheetsKeyed { unKeyBankBalanceSheets :: [BankBalanceSheetRow] }

instance FromJSON BankBalanceSheetsKeyed where
  parseJSON o = BankBalanceSheetsKeyed <$> (traverse parseJSON =<< createKeyedRows o)

newtype InsuranceBalanceSheetsKeyed = InsuranceBalanceSheetsKeyed { unKeyInsuranceBalanceSheets :: [InsuranceBalanceSheetRow] }

instance FromJSON InsuranceBalanceSheetsKeyed where
  parseJSON o = InsuranceBalanceSheetsKeyed <$> (traverse parseJSON =<< createKeyedRows o)

type IndustryBalanceSheetsKeyed
  = Industry GeneralBalanceSheetsKeyed BankBalanceSheetsKeyed InsuranceBalanceSheetsKeyed

type IndustryBalanceSheets
  = Industry [GeneralBalanceSheetRow] [BankBalanceSheetRow] [InsuranceBalanceSheetRow]

instance FromJSON (Industry GeneralBalanceSheetsKeyed BankBalanceSheetsKeyed InsuranceBalanceSheetsKeyed) where
  parseJSON json = General <$> parseJSON json
    <|> Bank <$> parseJSON json
    <|> Insurance <$> parseJSON json

unKeyIndustryBalanceSheets :: IndustryBalanceSheetsKeyed -> IndustryBalanceSheets
unKeyIndustryBalanceSheets = mapIndustry
  unKeyGeneralBalanceSheets
  unKeyBankBalanceSheets
  unKeyInsuranceBalanceSheets

fetchBalanceSheetsByTicker :: (MonadThrow m, MonadIO m) => SimFinContext -> [Text] -> FiscalPeriod -> Int -> m [IndustryBalanceSheets]
fetchBalanceSheetsByTicker ctx tickers period year =
  fmap unKeyIndustryBalanceSheets <$> performRequest ctx "companies/statements"
    [ ("ticker", Just $ T.encodeUtf8 $ T.intercalate "," tickers)
    , ("statement", Just "bs")
    , ("period", Just $ fiscalPeriodParam period)
    , ("fyear", Just $ BS8.pack $ show year)
    ]

------
-- P&L
------

data GeneralProfitAndLossRow
  = GeneralProfitAndLossRow
  { simFinId :: Int
  , ticker :: Text
  , fiscalPeriod :: String
  , fiscalYear :: Int
  , reportDate :: Day
  , publishDate :: Day
  , restatedDate :: Day
  , source :: Text
  , tTM :: Bool
  , valueCheck :: Bool
  , revenue :: Maybe Integer
  , salesAndServicesRevenue :: Maybe Integer
  , financingRevenue :: Maybe Integer
  , otherRevenue :: Maybe Integer
  , costOfRevenue :: Maybe Integer
  , costOfGoodsAndServices :: Maybe Integer
  , costOfFinancingRevenue :: Maybe Integer
  , costOfOtherRevenue :: Maybe Integer
  , grossProfit :: Maybe Integer
  , otherOperatingIncome :: Maybe Integer
  , operatingExpenses :: Maybe Integer
  , sellingGeneralAndAdministrative :: Maybe Integer
  , sellingAndMarketing :: Maybe Integer
  , generalAndAdministrative :: Maybe Integer
  , researchAndDevelopment :: Maybe Integer
  , depreciationAndAmortization :: Maybe Integer
  , provisionForDoubtfulAccounts :: Maybe Integer
  , otherOperatingExpenses :: Maybe Integer
  , operatingIncomeLoss :: Maybe Integer
  , nonOperatingIncomeLoss :: Maybe Integer
  , interestExpenseNet :: Maybe Integer
  , interestExpense :: Maybe Integer
  , interestIncome :: Maybe Integer
  , otherInvestmentIncomeLoss :: Maybe Integer
  , foreignExchangeGainLoss :: Maybe Integer
  , incomeLossFromAffiliates :: Maybe Integer
  , otherNonOperatingIncomeLoss :: Maybe Integer
  , pretaxIncomeLossAdj :: Maybe Integer
  , abnormalGainsLosses :: Maybe Integer
  , acquiredInProcessRAndD :: Maybe Integer
  , mergerAndAcquisitionExpense :: Maybe Integer
  , abnormalDerivatives :: Maybe Integer
  , disposalOfAssets :: Maybe Integer
  , earlyExtinguishmentOfDebt :: Maybe Integer
  , assetWriteDown :: Maybe Integer
  , impairmentOfGoodwillAndIntangibles :: Maybe Integer
  , saleOfBusiness :: Maybe Integer
  , legalSettlement :: Maybe Integer
  , restructuringCharges :: Maybe Integer
  , saleOfInvestmentsAndUnrealizedInvestments :: Maybe Integer
  , insuranceSettlement :: Maybe Integer
  , otherAbnormalItems :: Maybe Integer
  , pretaxIncomeLoss :: Maybe Integer
  , incomeTaxExpenseBenefitNet :: Maybe Integer
  , currentIncomeTax :: Maybe Integer
  , deferredIncomeTax :: Maybe Integer
  , taxAllowanceCredit :: Maybe Integer
  , incomeLossFromAffiliatesNetOfTaxes :: Maybe Integer
  , incomeLossFromContinuingOperations :: Maybe Integer
  , netExtraordinaryGainsLosses :: Maybe Integer
  , discontinuedOperations :: Maybe Integer
  , accountingChargesAndOther :: Maybe Integer
  , incomeLossInclMinorityInterest :: Maybe Integer
  , minorityInterest :: Maybe Integer
  , netIncome :: Maybe Integer
  , preferredDividends :: Maybe Integer
  , otherAdjustments :: Maybe Integer
  , netIncomeCommon :: Maybe Integer
  } deriving Show

data BankProfitAndLossRow
  = BankProfitAndLossRow
  { simFinId :: Int
  , ticker :: Text
  , fiscalPeriod :: String
  , fiscalYear :: Int
  , reportDate :: Day
  , publishDate :: Day
  , restatedDate :: Day
  , source :: Text
  , tTM :: Bool
  , valueCheck :: Bool
  , revenue :: Maybe Integer
  , netInterestIncome :: Maybe Integer
  , totalInterestIncome :: Maybe Integer
  , totalInterestExpense :: Maybe Integer
  , totalNonInterestIncome :: Maybe Integer
  , tradingAccountProfitsLosses :: Maybe Integer
  , investmentIncomeLoss :: Maybe Integer
  , saleOfLoanIncomeLoss :: Maybe Integer
  , commissionsAndFeesEarned :: Maybe Integer
  , netOTTILossesRecognisedInEarnings :: Maybe Integer
  , otherNonInterestIncome :: Maybe Integer
  , provisionForLoanLosses :: Maybe Integer
  , netRevenueAfterProvisions :: Maybe Integer
  , totalNonInterestExpense :: Maybe Integer
  , commissionsAndFeesPaid :: Maybe Integer
  , otherOperatingExpenses :: Maybe Integer
  , operatingIncomeLoss :: Maybe Integer
  , nonOperatingIncomeLoss :: Maybe Integer
  , incomeLossFromAffiliates :: Maybe Integer
  , otherNonOperatingIncomeLoss :: Maybe Integer
  , pretaxIncomeLossAdj :: Maybe Integer
  , abnormalGainsLosses :: Maybe Integer
  , debtValuationAdjustment :: Maybe Integer
  , creditValuationAdjustment :: Maybe Integer
  , mergerAndAcquisitionExpense :: Maybe Integer
  , disposalOfAssets :: Maybe Integer
  , earlyExtinguishmentOfDebt :: Maybe Integer
  , assetWriteDown :: Maybe Integer
  , impairmentOfGoodwillAndIntangibles :: Maybe Integer
  , saleOfBusiness :: Maybe Integer
  , legalSettlement :: Maybe Integer
  , restructuringCharges :: Maybe Integer
  , otherAbnormalItems :: Maybe Integer
  , pretaxIncomeLoss :: Maybe Integer
  , incomeTaxExpenseBenefitNet :: Maybe Integer
  , currentIncomeTax :: Maybe Integer
  , deferredIncomeTax :: Maybe Integer
  , taxAllowanceCredit :: Maybe Integer
  , incomeLossFromAffiliatesNetOfTaxes :: Maybe Integer
  , incomeLossFromContinuingOperations :: Maybe Integer
  , netExtraordinaryGainsLosses :: Maybe Integer
  , discontinuedOperations :: Maybe Integer
  , accountingChargesAndOther :: Maybe Integer
  , incomeLossInclMinorityInterest :: Maybe Integer
  , minorityInterest :: Maybe Integer
  , netIncome :: Maybe Integer
  , preferredDividends :: Maybe Integer
  , otherAdjustments :: Maybe Integer
  , netIncomeCommon :: Maybe Integer
  } deriving Show

data InsuranceProfitAndLossRow
  = InsuranceProfitAndLossRow
  { simFinId :: Int
  , ticker :: Text
  , fiscalPeriod :: String
  , fiscalYear :: Int
  , reportDate :: Day
  , publishDate :: Day
  , restatedDate :: Day
  , source :: Text
  , tTM :: Bool
  , valueCheck :: Bool
  , revenue :: Maybe Integer
  , netPremiumsEarned :: Maybe Integer
  , investmentIncomeLoss :: Maybe Integer
  , incomeFromRealEstate :: Maybe Integer
  , otherOperatingIncome :: Maybe Integer
  , policyChargesAndFees :: Maybe Integer
  , totalRealizedInvestmentGains :: Maybe Integer
  , totalOTTIRealized :: Maybe Integer
  , otherRealizedInvestmentGains :: Maybe Integer
  , otherIncome :: Maybe Integer
  , totalClaimsAndLosses :: Maybe Integer
  , claimsAndLosses :: Maybe Integer
  , longTermCharges :: Maybe Integer
  , otherClaimsAndLosses :: Maybe Integer
  , underwritingExpenseAndAcquisitionCost :: Maybe Integer
  , otherOperatingExpenses :: Maybe Integer
  , operatingIncomeLoss :: Maybe Integer
  , nonOperatingIncomeLoss :: Maybe Integer
  , incomeLossFromAffiliates :: Maybe Integer
  , interestExpenseNet :: Maybe Integer
  , otherNonOperatingIncomeLoss :: Maybe Integer
  , pretaxIncomeLossAdj :: Maybe Integer
  , abnormalGainsLosses :: Maybe Integer
  , mergerAndAcquisitionExpense :: Maybe Integer
  , abnormalDerivatives :: Maybe Integer
  , disposalOfAssets :: Maybe Integer
  , earlyExtinguishmentOfDebt :: Maybe Integer
  , assetWriteDown :: Maybe Integer
  , impairmentOfGoodwillAndIntangibles :: Maybe Integer
  , saleOfBusiness :: Maybe Integer
  , legalSettlement :: Maybe Integer
  , restructuringCharges :: Maybe Integer
  , netInvestmentLosses :: Maybe Integer
  , foreignExchange :: Maybe Integer
  , otherAbnormalItems :: Maybe Integer
  , pretaxIncomeLoss :: Maybe Integer
  , incomeTaxExpenseBenefitNet :: Maybe Integer
  , currentIncomeTax :: Maybe Integer
  , deferredIncomeTax :: Maybe Integer
  , taxAllowanceCredit :: Maybe Integer
  , incomeLossFromAffiliatesNetOfTaxes :: Maybe Integer
  , incomeLossFromContinuingOperations :: Maybe Integer
  , netExtraordinaryGainsLosses :: Maybe Integer
  , discontinuedOperations :: Maybe Integer
  , accountingChargesAndOther :: Maybe Integer
  , incomeLossInclMinorityInterest :: Maybe Integer
  , minorityInterest :: Maybe Integer
  , netIncome :: Maybe Integer
  , preferredDividends :: Maybe Integer
  , otherAdjustments :: Maybe Integer
  , netIncomeCommon :: Maybe Integer
  } deriving Show
  

instance FromJSON GeneralProfitAndLossRow where
  parseJSON = withObject "GeneralProfitAndLossRow" $ \v -> GeneralProfitAndLossRow
    <$> v .: "SimFinId"
    <*> v .: "Ticker"
    <*> v .: "Fiscal Period"
    <*> v .: "Fiscal Year"
    <*> v .: "Report Date"
    <*> v .: "Publish Date"
    <*> v .: "Restated Date"
    <*> v .: "Source"
    <*> v .: "TTM"
    <*> v .: "Value Check"
    <*> v .: "Revenue"
    <*> v .: "Sales & Services Revenue"
    <*> v .: "Financing Revenue"
    <*> v .: "Other Revenue"
    <*> v .: "Cost of Revenue"
    <*> v .: "Cost of Goods & Services"
    <*> v .: "Cost of Financing Revenue"
    <*> v .: "Cost of Other Revenue"
    <*> v .: "Gross Profit"
    <*> v .: "Other Operating Income"
    <*> v .: "Operating Expenses"
    <*> v .: "Selling, General & Administrative"
    <*> v .: "Selling & Marketing"
    <*> v .: "General & Administrative"
    <*> v .: "Research & Development"
    <*> v .: "Depreciation & Amortization"
    <*> v .: "Provision for Doubtful Accounts"
    <*> v .: "Other Operating Expenses"
    <*> v .: "Operating Income (Loss)"
    <*> v .: "Non-Operating Income (Loss)"
    <*> v .: "Interest Expense, Net"
    <*> v .: "Interest Expense"
    <*> v .: "Interest Income"
    <*> v .: "Other Investment Income (Loss)"
    <*> v .: "Foreign Exchange Gain (Loss)"
    <*> v .: "Income (Loss) from Affiliates"
    <*> v .: "Other Non-Operating Income (Loss)"
    <*> v .: "Pretax Income (Loss), Adj."
    <*> v .: "Abnormal Gains (Losses)"
    <*> v .: "Acquired In-Process R&D"
    <*> v .: "Merger & Acquisition Expense"
    <*> v .: "Abnormal Derivatives"
    <*> v .: "Disposal of Assets"
    <*> v .: "Early Extinguishment of Debt"
    <*> v .: "Asset Write-Down"
    <*> v .: "Impairment of Goodwill & Intangibles"
    <*> v .: "Sale of Business"
    <*> v .: "Legal Settlement"
    <*> v .: "Restructuring Charges"
    <*> v .: "Sale of Investments & Unrealized Investments"
    <*> v .: "Insurance Settlement"
    <*> v .: "Other Abnormal Items"
    <*> v .: "Pretax Income (Loss)"
    <*> v .: "Income Tax (Expense) Benefit, Net"
    <*> v .: "Current Income Tax"
    <*> v .: "Deferred Income Tax"
    <*> v .: "Tax Allowance/Credit"
    <*> v .: "Income (Loss) from Affiliates, Net of Taxes"
    <*> v .: "Income (Loss) from Continuing Operations"
    <*> v .: "Net Extraordinary Gains (Losses)"
    <*> v .: "Discontinued Operations"
    <*> v .: "Accounting Charges & Other"
    <*> v .: "Income (Loss) Incl. Minority Interest"
    <*> v .: "Minority Interest"
    <*> v .: "Net Income"
    <*> v .: "Preferred Dividends"
    <*> v .: "Other Adjustments"
    <*> v .: "Net Income (Common)"

instance FromJSON BankProfitAndLossRow where
  parseJSON = withObject "BankProfitAndLossRow" $ \v -> BankProfitAndLossRow
    <$> v .: "SimFinId"
    <*> v .: "Ticker"
    <*> v .: "Fiscal Period"
    <*> v .: "Fiscal Year"
    <*> v .: "Report Date"
    <*> v .: "Publish Date"
    <*> v .: "Restated Date"
    <*> v .: "Source"
    <*> v .: "TTM"
    <*> v .: "Value Check"
    <*> v .: "Revenue"
    <*> v .: "Net Interest Income"
    <*> v .: "Total Interest Income"
    <*> v .: "Total Interest Expense"
    <*> v .: "Total Non-Interest Income"
    <*> v .: "Trading Account Profits/Losses"
    <*> v .: "Investment Income (Loss)"
    <*> v .: "Sale of Loan Income (Loss)"
    <*> v .: "Commissions & Fees Earned"
    <*> v .: "Net OTTI Losses Recognised in Earnings"
    <*> v .: "Other Non-Interest Income"
    <*> v .: "Provision for Loan Losses"
    <*> v .: "Net Revenue after Provisions"
    <*> v .: "Total Non-Interest Expense"
    <*> v .: "Commissions & Fees Paid"
    <*> v .: "Other Operating Expenses"
    <*> v .: "Operating Income (Loss)"
    <*> v .: "Non-Operating Income (Loss)"
    <*> v .: "Income (Loss) from Affiliates"
    <*> v .: "Other Non-Operating Income (Loss)"
    <*> v .: "Pretax Income (Loss), Adj."
    <*> v .: "Abnormal Gains (Losses)"
    <*> v .: "Debt Valuation Adjustment"
    <*> v .: "Credit Valuation Adjustment"
    <*> v .: "Merger & Acquisition Expense"
    <*> v .: "Disposal of Assets"
    <*> v .: "Early Extinguishment of Debt"
    <*> v .: "Asset Write-Down"
    <*> v .: "Impairment of Goodwill & Intangibles"
    <*> v .: "Sale of Business"
    <*> v .: "Legal Settlement"
    <*> v .: "Restructuring Charges"
    <*> v .: "Other Abnormal Items"
    <*> v .: "Pretax Income (Loss)"
    <*> v .: "Income Tax (Expense) Benefit, Net"
    <*> v .: "Current Income Tax"
    <*> v .: "Deferred Income Tax"
    <*> v .: "Tax Allowance/Credit"
    <*> v .: "Income (Loss) from Affiliates, Net of Taxes"
    <*> v .: "Income (Loss) from Continuing Operations"
    <*> v .: "Net Extraordinary Gains (Losses)"
    <*> v .: "Discontinued Operations"
    <*> v .: "Accounting Charges & Other"
    <*> v .: "Income (Loss) Incl. Minority Interest"
    <*> v .: "Minority Interest"
    <*> v .: "Net Income"
    <*> v .: "Preferred Dividends"
    <*> v .: "Other Adjustments"
    <*> v .: "Net Income (Common)"

instance FromJSON InsuranceProfitAndLossRow where
  parseJSON = withObject "InsuranceProfitAndLossRow" $ \v -> InsuranceProfitAndLossRow
    <$> v .: "SimFinId"
    <*> v .: "Ticker"
    <*> v .: "Fiscal Period"
    <*> v .: "Fiscal Year"
    <*> v .: "Report Date"
    <*> v .: "Publish Date"
    <*> v .: "Restated Date"
    <*> v .: "Source"
    <*> v .: "TTM"
    <*> v .: "Value Check"
    <*> v .: "Revenue"
    <*> v .: "Net Premiums Earned"
    <*> v .: "Investment Income (Loss)"
    <*> v .: "Income from Real Estate"
    <*> v .: "Other Operating Income"
    <*> v .: "Policy Charges & Fees"
    <*> v .: "Total Realized Investment Gains"
    <*> v .: "Total OTTI Realized"
    <*> v .: "Other Realized Investment Gains"
    <*> v .: "Other Income"
    <*> v .: "Total Claims & Losses"
    <*> v .: "Claims & Losses"
    <*> v .: "Long Term Charges"
    <*> v .: "Other Claims & Losses"
    <*> v .: "Underwriting Expense & Acquisition Cost"
    <*> v .: "Other Operating Expenses"
    <*> v .: "Operating Income (Loss)"
    <*> v .: "Non-Operating Income (Loss)"
    <*> v .: "Income (Loss) from Affiliates"
    <*> v .: "Interest Expense, Net"
    <*> v .: "Other Non-Operating Income (Loss)"
    <*> v .: "Pretax Income (Loss), Adj."
    <*> v .: "Abnormal Gains (Losses)"
    <*> v .: "Merger & Acquisition Expense"
    <*> v .: "Abnormal Derivatives"
    <*> v .: "Disposal of Assets"
    <*> v .: "Early Extinguishment of Debt"
    <*> v .: "Asset Write-Down"
    <*> v .: "Impairment of Goodwill & Intangibles"
    <*> v .: "Sale of Business"
    <*> v .: "Legal Settlement"
    <*> v .: "Restructuring Charges"
    <*> v .: "Net Investment Losses"
    <*> v .: "Foreign Exchange"
    <*> v .: "Other Abnormal Items"
    <*> v .: "Pretax Income (Loss)"
    <*> v .: "Income Tax (Expense) Benefit, Net"
    <*> v .: "Current Income Tax"
    <*> v .: "Deferred Income Tax"
    <*> v .: "Tax Allowance/Credit"
    <*> v .: "Income (Loss) from Affiliates, Net of Taxes"
    <*> v .: "Income (Loss) from Continuing Operations"
    <*> v .: "Net Extraordinary Gains (Losses)"
    <*> v .: "Discontinued Operations"
    <*> v .: "Accounting Charges & Other"
    <*> v .: "Income (Loss) Incl. Minority Interest"
    <*> v .: "Minority Interest"
    <*> v .: "Net Income"
    <*> v .: "Preferred Dividends"
    <*> v .: "Other Adjustments"
    <*> v .: "Net Income (Common)"

newtype GeneralProfitsAndLossesKeyed = GeneralProfitsAndLossesKeyed { unKeyGeneralProfitsAndLosses :: [GeneralProfitAndLossRow] }

instance FromJSON GeneralProfitsAndLossesKeyed where
  parseJSON o = GeneralProfitsAndLossesKeyed <$> (traverse parseJSON =<< createKeyedRows o)

newtype BankProfitsAndLossesKeyed = BankProfitsAndLossesKeyed { unKeyBankProfitsAndLosses :: [BankProfitAndLossRow] }

instance FromJSON BankProfitsAndLossesKeyed where
  parseJSON o = BankProfitsAndLossesKeyed <$> (traverse parseJSON =<< createKeyedRows o)

newtype InsuranceProfitsAndLossesKeyed = InsuranceProfitsAndLossesKeyed { unKeyInsuranceProfitsAndLosses :: [InsuranceProfitAndLossRow] }

instance FromJSON InsuranceProfitsAndLossesKeyed where
  parseJSON o = InsuranceProfitsAndLossesKeyed <$> (traverse parseJSON =<< createKeyedRows o)

type IndustryProfitsAndLossesKeyed
  = Industry GeneralProfitsAndLossesKeyed BankProfitsAndLossesKeyed InsuranceProfitsAndLossesKeyed

type IndustryProfitsAndLosses
  = Industry [GeneralProfitAndLossRow] [BankProfitAndLossRow] [InsuranceProfitAndLossRow]

instance FromJSON (Industry GeneralProfitsAndLossesKeyed BankProfitsAndLossesKeyed InsuranceProfitsAndLossesKeyed) where
  parseJSON json = General <$> parseJSON json
    <|> Insurance <$> parseJSON json
    <|> Bank <$> parseJSON json

unKeyIndustryProfitsAndLosses :: IndustryProfitsAndLossesKeyed -> IndustryProfitsAndLosses
unKeyIndustryProfitsAndLosses = mapIndustry
  unKeyGeneralProfitsAndLosses
  unKeyBankProfitsAndLosses
  unKeyInsuranceProfitsAndLosses

fetchProfitsAndLossesByTicker :: (MonadThrow m, MonadIO m) => SimFinContext -> [Text] -> FiscalPeriod -> Int -> m [IndustryProfitsAndLosses]
fetchProfitsAndLossesByTicker ctx tickers period year =
  fmap unKeyIndustryProfitsAndLosses <$> performRequest ctx "companies/statements"
    [ ("ticker", Just $ T.encodeUtf8 $ T.intercalate "," tickers)
    , ("statement", Just "pl")
    , ("period", Just $ fiscalPeriodParam period)
    , ("fyear", Just $ BS8.pack $ show year)
    ]

-----
-- Cash Flows
------

data GeneralCashFlowRow
  = GeneralCashFlowRow
  { simFinId :: Int
  , ticker :: Text
  , fiscalPeriod :: String
  , fiscalYear :: Int
  , reportDate :: Day
  , publishDate :: Day
  , restatedDate :: Day
  , source :: Text
  , tTM :: Bool
  , valueCheck :: Bool
  , netIncomeStartingLine :: Maybe Integer
  , netIncome :: Maybe Integer
  , netIncomeFromDiscontinuedOperations :: Maybe Integer
  , otherAdjustments :: Maybe Integer
  , depreciationAndAmortization :: Maybe Integer
  , nonCashItems :: Maybe Integer
  , stockBasedCompensation :: Maybe Integer
  , deferredIncomeTaxes :: Maybe Integer
  , otherNonCashAdjustments :: Maybe Integer
  , changeInWorkingCapital :: Maybe Integer
  , changeInAccountsReceivable :: Maybe Integer
  , changeInInventories :: Maybe Integer
  , changeInAccountsPayable :: Maybe Integer
  , changeInOther :: Maybe Integer
  , netCashFromDiscontinuedOperationsOperating :: Maybe Integer
  , netCashFromOperatingActivities :: Maybe Integer
  , changeInFixedAssetsAndIntangibles :: Maybe Integer
  , dispositionOfFixedAssetsAndIntangibles :: Maybe Integer
  , dispositionOfFixedAssets :: Maybe Integer
  , dispositionOfIntangibleAssets :: Maybe Integer
  , acquisitionOfFixedAssetsAndIntangibles :: Maybe Integer
  , purchaseOfFixedAssets :: Maybe Integer
  , acquisitionOfIntangibleAssets :: Maybe Integer
  , otherChangeInFixedAssetsAndIntangibles :: Maybe Integer
  , netChangeInLongTermInvestment :: Maybe Integer
  , decreaseInLongTermInvestment :: Maybe Integer
  , increaseInLongTermInvestment :: Maybe Integer
  , netCashFromAcquisitionsAndDivestitures :: Maybe Integer
  , netCashFromDivestitures :: Maybe Integer
  , cashForAcquisitionOfSubsidiaries :: Maybe Integer
  , cashForJointVentures :: Maybe Integer
  , netCashFromOtherAcquisitions :: Maybe Integer
  , otherInvestingActivities :: Maybe Integer
  , netCashFromDiscontinuedOperationsInvesting :: Maybe Integer
  , netCashFromInvestingActivities :: Maybe Integer
  , dividendsPaid :: Maybe Integer
  , cashFromRepaymentOfDebt :: Maybe Integer
  , cashFromRepaymentOfShortTermDebtNet :: Maybe Integer
  , cashFromRepaymentOfLongTermDebtNet :: Maybe Integer
  , repaymentsOfLongTermDebt :: Maybe Integer
  , cashFromLongTermDebt :: Maybe Integer
  , cashFromRepurchaseOfEquity :: Maybe Integer
  , increaseInCapitalStock :: Maybe Integer
  , decreaseInCapitalStock :: Maybe Integer
  , otherFinancingActivities :: Maybe Integer
  , netCashFromDiscontinuedOperationsFinancing :: Maybe Integer
  , netCashFromFinancingActivities :: Maybe Integer
  , netCashBeforeDiscOperationsAndFX :: Maybe Integer
  , changeInCashFromDiscOperationsAndOther :: Maybe Integer
  , netCashBeforeFX :: Maybe Integer
  , effectOfForeignExchangeRates :: Maybe Integer
  , netChangeInCash :: Maybe Integer
  } deriving Show

data BankCashFlowRow
  = BankCashFlowRow
  { simFinId :: Int
  , ticker :: Text
  , fiscalPeriod :: String
  , fiscalYear :: Int
  , reportDate :: Day
  , publishDate :: Day
  , restatedDate :: Day
  , source :: Text
  , tTM :: Bool
  , valueCheck :: Bool
  , netIncomeStartingLine :: Maybe Integer
  , netIncome :: Maybe Integer
  , netIncomeFromDiscontinuedOperations :: Maybe Integer
  , otherAdjustments :: Maybe Integer
  , depreciationAndAmortization :: Maybe Integer
  , provisionForLoanLosses :: Maybe Integer
  , nonCashItems :: Maybe Integer
  , gainOnSaleOfSecuritiesAndLoans :: Maybe Integer
  , deferredIncomeTaxes :: Maybe Integer
  , stockBasedCompensation :: Maybe Integer
  , otherNonCashAdjustments :: Maybe Integer
  , changeInWorkingCapital :: Maybe Integer
  , tradingAssetsAndLiabilities :: Maybe Integer
  , netChangeOfInvestments :: Maybe Integer
  , netChangeOfInterbankAssets :: Maybe Integer
  , netChangeOfInterbankLiabilities :: Maybe Integer
  , netChangeInOperatingLoans :: Maybe Integer
  , accruedInterestReceivable :: Maybe Integer
  , accruedInterestPayable :: Maybe Integer
  , otherOperatingAssetsLiabilities :: Maybe Integer
  , netCashFromDiscontinuedOperationsOperating :: Maybe Integer
  , netCashFromOperatingActivities :: Maybe Integer
  , changeInFixedAssetsAndIntangibles :: Maybe Integer
  , dispositionOfFixedAssetsAndIntangibles :: Maybe Integer
  , capitalExpenditures :: Maybe Integer
  , netChangeInInvestments :: Maybe Integer
  , decreaseInInvestments :: Maybe Integer
  , decreaseInHTMInvestments :: Maybe Integer
  , decreaseInAFSInvestments :: Maybe Integer
  , increaseInInvestments :: Maybe Integer
  , increaseInHTMInvestments :: Maybe Integer
  , increaseInAFSInvestments :: Maybe Integer
  , netChangeInOtherInvestments :: Maybe Integer
  , netChangeInLoansAndInterbank :: Maybe Integer
  , netChangeInCustomerLoans :: Maybe Integer
  , netChangeInInterbankAssets :: Maybe Integer
  , netChangeInOtherLoans :: Maybe Integer
  , netCashFromAcquisitionsAndDivestitures :: Maybe Integer
  , netCashFromDivestitures :: Maybe Integer
  , cashForAcquisitionOfSubsidiaries :: Maybe Integer
  , cashForJointVentures :: Maybe Integer
  , netCashFromOtherAcquisitions :: Maybe Integer
  , otherInvestingActivities :: Maybe Integer
  , netCashFromDiscontinuedOperationsInvesting :: Maybe Integer
  , netCashFromInvestingActivities :: Maybe Integer
  , dividendsPaid :: Maybe Integer
  , cashFromRepaymentOfDebt :: Maybe Integer
  , cashFromRepaymentOfShortTermDebtNet :: Maybe Integer
  , netChangeInInterbankTransfers :: Maybe Integer
  , cashFromRepaymentOfLongTermDebtNet :: Maybe Integer
  , repaymentsOfLongTermDebt :: Maybe Integer
  , cashFromLongTermDebt :: Maybe Integer
  , cashFromRepurchaseOfEquity :: Maybe Integer
  , increaseInCapitalStock :: Maybe Integer
  , decreaseInCapitalStock :: Maybe Integer
  , netChangeInDeposits :: Maybe Integer
  , otherFinancingActivities :: Maybe Integer
  , netCashFromDiscontinuedOperationsFinancing :: Maybe Integer
  , netCashFromFinancingActivities :: Maybe Integer
  , netCashBeforeDiscOperationsAndFX :: Maybe Integer
  , changeInCashFromDiscOperationsAndOther :: Maybe Integer
  , netCashBeforeFX :: Maybe Integer
  , effectOfForeignExchangeRates :: Maybe Integer
  , netChangeInCash :: Maybe Integer
  } deriving Show

data InsuranceCashFlowRow
  = InsuranceCashFlowRow
  { simFinId :: Int
  , ticker :: Text
  , fiscalPeriod :: String
  , fiscalYear :: Int
  , reportDate :: Day
  , publishDate :: Day
  , restatedDate :: Day
  , source :: Text
  , tTM :: Bool
  , valueCheck :: Bool
  , netIncomeStartingLine :: Maybe Integer
  , netIncome :: Maybe Integer
  , netIncomeFromDiscontinuedOperations :: Maybe Integer
  , otherAdjustments :: Maybe Integer
  , depreciationAndAmortization :: Maybe Integer
  , nonCashItems :: Maybe Integer
  , stockBasedCompensation :: Maybe Integer
  , deferredIncomeTaxes :: Maybe Integer
  , otherNonCashAdjustments :: Maybe Integer
  , changeInWorkingCapital :: Maybe Integer
  , netCashFromDiscontinuedOperationsOperating :: Maybe Integer
  , netCashFromOperatingActivities :: Maybe Integer
  , changeInFixedAssetsAndIntangibles :: Maybe Integer
  , dispositionOfFixedAssetsAndIntangibles :: Maybe Integer
  , acquisitionOfFixedAssetsAndIntangibles :: Maybe Integer
  , netChangeInInvestments :: Maybe Integer
  , increaseInInvestments :: Maybe Integer
  , decreaseInInvestments :: Maybe Integer
  , otherInvestingActivities :: Maybe Integer
  , netCashFromDiscontinuedOperationsInvesting :: Maybe Integer
  , netCashFromInvestingActivities :: Maybe Integer
  , dividendsPaid :: Maybe Integer
  , cashFromRepaymentOfDebt :: Maybe Integer
  , cashFromRepaymentOfShortTermDebtNet :: Maybe Integer
  , cashFromRepaymentOfLongTermDebtNet :: Maybe Integer
  , repaymentsOfLongTermDebt :: Maybe Integer
  , cashFromLongTermDebt :: Maybe Integer
  , cashFromRepurchaseOfEquity :: Maybe Integer
  , increaseInCapitalStock :: Maybe Integer
  , decreaseInCapitalStock :: Maybe Integer
  , changeInInsuranceReserves :: Maybe Integer
  , otherFinancingActivities :: Maybe Integer
  , netCashFromDiscontinuedOperationsFinancing :: Maybe Integer
  , netCashFromFinancingActivities :: Maybe Integer
  , netCashBeforeDiscOperationsAndFX :: Maybe Integer
  , changeInCashFromDiscOperationsAndOther :: Maybe Integer
  , netCashBeforeFX :: Maybe Integer
  , effectOfForeignExchangeRates :: Maybe Integer
  , netChangeInCash :: Maybe Integer
  } deriving Show

instance FromJSON GeneralCashFlowRow where
  parseJSON = withObject "GeneralCashFlowRow" $ \v -> GeneralCashFlowRow
    <$> v .: "SimFinId"
    <*> v .: "Ticker"
    <*> v .: "Fiscal Period"
    <*> v .: "Fiscal Year"
    <*> v .: "Report Date"
    <*> v .: "Publish Date"
    <*> v .: "Restated Date"
    <*> v .: "Source"
    <*> v .: "TTM"
    <*> v .: "Value Check"
    <*> v .: "Net Income/Starting Line"
    <*> v .: "Net Income"
    <*> v .: "Net Income from Discontinued Operations"
    <*> v .: "Other Adjustments"
    <*> v .: "Depreciation & Amortization"
    <*> v .: "Non-Cash Items"
    <*> v .: "Stock-Based Compensation"
    <*> v .: "Deferred Income Taxes"
    <*> v .: "Other Non-Cash Adjustments"
    <*> v .: "Change in Working Capital"
    <*> v .: "Change in Accounts Receivable"
    <*> v .: "Change in Inventories"
    <*> v .: "Change in Accounts Payable"
    <*> v .: "Change in Other"
    <*> v .: "Net Cash from Discontinued Operations (Operating)"
    <*> v .: "Net Cash from Operating Activities"
    <*> v .: "Change in Fixed Assets & Intangibles"
    <*> v .: "Disposition of Fixed Assets & Intangibles"
    <*> v .: "Disposition of Fixed Assets"
    <*> v .: "Disposition of Intangible Assets"
    <*> v .: "Acquisition of Fixed Assets & Intangibles"
    <*> v .: "Purchase of Fixed Assets"
    <*> v .: "Acquisition of Intangible Assets"
    <*> v .: "Other Change in Fixed Assets & Intangibles"
    <*> v .: "Net Change in Long Term Investment"
    <*> v .: "Decrease in Long Term Investment"
    <*> v .: "Increase in Long Term Investment"
    <*> v .: "Net Cash from Acquisitions & Divestitures"
    <*> v .: "Net Cash from Divestitures"
    <*> v .: "Cash for Acquisition of Subsidiaries"
    <*> v .: "Cash for Joint Ventures"
    <*> v .: "Net Cash from Other Acquisitions"
    <*> v .: "Other Investing Activities"
    <*> v .: "Net Cash from Discontinued Operations (Investing)"
    <*> v .: "Net Cash from Investing Activities"
    <*> v .: "Dividends Paid"
    <*> v .: "Cash from (Repayment of) Debt"
    <*> v .: "Cash from (Repayment of) Short Term Debt, Net"
    <*> v .: "Cash from (Repayment of) Long Term Debt, Net"
    <*> v .: "Repayments of Long Term Debt"
    <*> v .: "Cash from Long Term Debt"
    <*> v .: "Cash from (Repurchase of) Equity"
    <*> v .: "Increase in Capital Stock"
    <*> v .: "Decrease in Capital Stock"
    <*> v .: "Other Financing Activities"
    <*> v .: "Net Cash from Discontinued Operations (Financing)"
    <*> v .: "Net Cash from Financing Activities"
    <*> v .: "Net Cash Before Disc. Operations and FX"
    <*> v .: "Change in Cash from Disc. Operations and Other"
    <*> v .: "Net Cash Before FX"
    <*> v .: "Effect of Foreign Exchange Rates"
    <*> v .: "Net Change in Cash"

instance FromJSON BankCashFlowRow where
  parseJSON = withObject "BankCashFlowRow" $ \v -> BankCashFlowRow
    <$> v .: "SimFinId"
    <*> v .: "Ticker"
    <*> v .: "Fiscal Period"
    <*> v .: "Fiscal Year"
    <*> v .: "Report Date"
    <*> v .: "Publish Date"
    <*> v .: "Restated Date"
    <*> v .: "Source"
    <*> v .: "TTM"
    <*> v .: "Value Check"
    <*> v .: "Net Income/Starting Line"
    <*> v .: "Net Income"
    <*> v .: "Net Income from Discontinued Operations"
    <*> v .: "Other Adjustments"
    <*> v .: "Depreciation & Amortization"
    <*> v .: "Provision for Loan Losses"
    <*> v .: "Non-Cash Items"
    <*> v .: "Gain On Sale of Securities & Loans"
    <*> v .: "Deferred Income Taxes"
    <*> v .: "Stock-Based Compensation"
    <*> v .: "Other Non-Cash Adjustments"
    <*> v .: "Change in Working Capital"
    <*> v .: "Trading Assets & Liabilities"
    <*> v .: "Net Change of Investments"
    <*> v .: "Net Change of Interbank Assets"
    <*> v .: "Net Change of Interbank Liabilities"
    <*> v .: "Net Change in Operating Loans"
    <*> v .: "Accrued Interest Receivable"
    <*> v .: "Accrued Interest Payable"
    <*> v .: "Other Operating Assets/Liabilities"
    <*> v .: "Net Cash from Discontinued Operations (Operating)"
    <*> v .: "Net Cash from Operating Activities"
    <*> v .: "Change in Fixed Assets & Intangibles"
    <*> v .: "Disposition of Fixed Assets & Intangibles"
    <*> v .: "Capital Expenditures"
    <*> v .: "Net Change in Investments"
    <*> v .: "Decrease in Investments"
    <*> v .: "Decrease in HTM Investments"
    <*> v .: "Decrease in AFS Investments"
    <*> v .: "Increase in Investments"
    <*> v .: "Increase in HTM Investments"
    <*> v .: "Increase in AFS Investments"
    <*> v .: "Net Change in Other Investments"
    <*> v .: "Net Change in Loans & Interbank"
    <*> v .: "Net Change in Customer Loans"
    <*> v .: "Net Change in Interbank Assets"
    <*> v .: "Net Change in Other Loans"
    <*> v .: "Net Cash from Acquisitions & Divestitures"
    <*> v .: "Net Cash from Divestitures"
    <*> v .: "Cash for Acquisition of Subsidiaries"
    <*> v .: "Cash for Joint Ventures"
    <*> v .: "Net Cash from Other Acquisitions"
    <*> v .: "Other Investing Activities"
    <*> v .: "Net Cash from Discontinued Operations (Investing)"
    <*> v .: "Net Cash from Investing Activities"
    <*> v .: "Dividends Paid"
    <*> v .: "Cash from (Repayment of) Debt"
    <*> v .: "Cash from (Repayment of) Short Term Debt, Net"
    <*> v .: "Net Change in Interbank Transfers"
    <*> v .: "Cash from (Repayment of) Long Term Debt, Net"
    <*> v .: "Repayments of Long Term Debt"
    <*> v .: "Cash from Long Term Debt"
    <*> v .: "Cash from (Repurchase of) Equity"
    <*> v .: "Increase in Capital Stock"
    <*> v .: "Decrease in Capital Stock"
    <*> v .: "Net Change In Deposits"
    <*> v .: "Other Financing Activities"
    <*> v .: "Net Cash from Discontinued Operations (Financing)"
    <*> v .: "Net Cash from Financing Activities"
    <*> v .: "Net Cash Before Disc. Operations and FX"
    <*> v .: "Change in Cash from Disc. Operations and Other"
    <*> v .: "Net Cash Before FX"
    <*> v .: "Effect of Foreign Exchange Rates"
    <*> v .: "Net Change in Cash"

instance FromJSON InsuranceCashFlowRow where
  parseJSON = withObject "InsuranceCashFlowRow" $ \v -> InsuranceCashFlowRow
    <$> v .: "SimFinId"
    <*> v .: "Ticker"
    <*> v .: "Fiscal Period"
    <*> v .: "Fiscal Year"
    <*> v .: "Report Date"
    <*> v .: "Publish Date"
    <*> v .: "Restated Date"
    <*> v .: "Source"
    <*> v .: "TTM"
    <*> v .: "Value Check"
    <*> v .: "Net Income/Starting Line"
    <*> v .: "Net Income"
    <*> v .: "Net Income from Discontinued Operations"
    <*> v .: "Other Adjustments"
    <*> v .: "Depreciation & Amortization"
    <*> v .: "Non-Cash Items"
    <*> v .: "Stock-Based Compensation"
    <*> v .: "Deferred Income Taxes"
    <*> v .: "Other Non-Cash Adjustments"
    <*> v .: "Change in Working Capital"
    <*> v .: "Net Cash from Discontinued Operations (Operating)"
    <*> v .: "Net Cash from Operating Activities"
    <*> v .: "Change in Fixed Assets & Intangibles"
    <*> v .: "Disposition of Fixed Assets & Intangibles"
    <*> v .: "Acquisition of Fixed Assets & Intangibles"
    <*> v .: "Net Change in Investments"
    <*> v .: "Increase in Investments"
    <*> v .: "Decrease in Investments"
    <*> v .: "Other Investing Activities"
    <*> v .: "Net Cash from Discontinued Operations (Investing)"
    <*> v .: "Net Cash from Investing Activities"
    <*> v .: "Dividends Paid"
    <*> v .: "Cash from (Repayment of) Debt"
    <*> v .: "Cash from (Repayment of) Short Term Debt, Net"
    <*> v .: "Cash from (Repayment of) Long Term Debt, Net"
    <*> v .: "Repayments of Long Term Debt"
    <*> v .: "Cash from Long Term Debt"
    <*> v .: "Cash from (Repurchase of) Equity"
    <*> v .: "Increase in Capital Stock"
    <*> v .: "Decrease in Capital Stock"
    <*> v .: "Change in Insurance Reserves"
    <*> v .: "Other Financing Activities"
    <*> v .: "Net Cash from Discontinued Operations (Financing)"
    <*> v .: "Net Cash from Financing Activities"
    <*> v .: "Net Cash Before Disc. Operations and FX"
    <*> v .: "Change in Cash from Disc. Operations and Other"
    <*> v .: "Net Cash Before FX"
    <*> v .: "Effect of Foreign Exchange Rates"
    <*> v .: "Net Change in Cash"

newtype GeneralCashFlowsKeyed = GeneralCashFlowsKeyed { unKeyGeneralCashFlows :: [GeneralCashFlowRow] }

instance FromJSON GeneralCashFlowsKeyed where
  parseJSON o = GeneralCashFlowsKeyed <$> (traverse parseJSON =<< createKeyedRows o)

newtype BankCashFlowsKeyed = BankCashFlowsKeyed { unKeyBankCashFlows :: [BankCashFlowRow] }

instance FromJSON BankCashFlowsKeyed where
  parseJSON o = BankCashFlowsKeyed <$> (traverse parseJSON =<< createKeyedRows o)

newtype InsuranceCashFlowsKeyed = InsuranceCashFlowsKeyed { unKeyInsuranceCashFlows :: [InsuranceCashFlowRow] }

instance FromJSON InsuranceCashFlowsKeyed where
  parseJSON o = InsuranceCashFlowsKeyed <$> (traverse parseJSON =<< createKeyedRows o)

type IndustryCashFlowsKeyed
  = Industry GeneralCashFlowsKeyed BankCashFlowsKeyed InsuranceCashFlowsKeyed

type IndustryCashFlows
  = Industry [GeneralCashFlowRow] [BankCashFlowRow] [InsuranceCashFlowRow]

instance FromJSON (Industry GeneralCashFlowsKeyed BankCashFlowsKeyed InsuranceCashFlowsKeyed) where
  parseJSON json = Insurance <$> parseJSON json
    <|> Bank <$> parseJSON json
    <|> General <$> parseJSON json

unKeyIndustryCashFlows :: IndustryCashFlowsKeyed -> IndustryCashFlows
unKeyIndustryCashFlows = mapIndustry
  unKeyGeneralCashFlows
  unKeyBankCashFlows
  unKeyInsuranceCashFlows

fetchCashFlowsByTicker :: (MonadThrow m, MonadIO m) => SimFinContext -> [Text] -> FiscalPeriod -> Int -> m [IndustryCashFlows]
fetchCashFlowsByTicker ctx tickers period year =
  fmap unKeyIndustryCashFlows <$> performRequest ctx "companies/statements"
    [ ("ticker", Just $ T.encodeUtf8 $ T.intercalate "," tickers)
    , ("statement", Just "cf")
    , ("period", Just $ fiscalPeriodParam period)
    , ("fyear", Just $ BS8.pack $ show year)
    ]

------
-- Derived
------

newtype StringFrac a = StringFrac { unFloat :: a }
  deriving Show

instance (Read a, RealFrac a) => FromJSON (StringFrac a) where
  parseJSON (String s) = pure $ StringFrac $ read $ T.unpack s
  parseJSON (Number n) = pure $ StringFrac $ realToFrac n
  parseJSON v = typeMismatch "Number or String" v

data DerivedRow a
  = DerivedRow
  { simFinId :: Int
  , ticker :: Text
  , fiscalPeriod :: String
  , fiscalYear :: Int
  , reportDate :: Day
  , publishDate :: Day
  , restatedDate :: Day
  , source :: Text
  , tTM :: Bool
  , valueCheck :: Bool
  , eBITDA :: Maybe (StringFrac a)
  , totalDebt :: Maybe (StringFrac a)
  , freeCashFlow :: Maybe (StringFrac a)
  , grossProfitMargin :: Maybe (StringFrac a)
  , operatingMargin :: Maybe (StringFrac a)
  , netProfitMargin :: Maybe (StringFrac a)
  , returnOnEquity :: Maybe (StringFrac a)
  , returnOnAssets :: Maybe (StringFrac a)
  , freeCashFlowToNetIncome :: Maybe (StringFrac a)
  , currentRatio :: Maybe (StringFrac a)
  , liabilitiesToEquityRatio :: Maybe (StringFrac a)
  , debtRatio :: Maybe (StringFrac a)
  , earningsPerShareBasic :: Maybe (StringFrac a)
  , earningsPerShareDiluted :: Maybe (StringFrac a)
  , salesPerShare :: Maybe (StringFrac a)
  , equityPerShare :: Maybe (StringFrac a)
  , freeCashFlowPerShare :: Maybe (StringFrac a)
  , dividendsPerShare :: Maybe (StringFrac a)
  , piotroskiFScore :: Maybe Int
  , returnOnInvestedCapital :: Maybe (StringFrac a)
  , cashReturnOnInvestedCapital :: Maybe (StringFrac a)
  , dividendPayoutRatio :: Maybe (StringFrac a)
  , netDebtEBITDA :: Maybe (StringFrac a)
  , netDebtEBIT :: Maybe (StringFrac a)
  } deriving Show

instance (Read a, RealFrac a) => FromJSON (DerivedRow a) where
  parseJSON = withObject "DerivedRow" $ \v -> DerivedRow
    <$> v .: "SimFinId"
    <*> v .: "Ticker"
    <*> v .: "Fiscal Period"
    <*> v .: "Fiscal Year"
    <*> v .: "Report Date"
    <*> v .: "Publish Date"
    <*> v .: "Restated Date"
    <*> v .: "Source"
    <*> v .: "TTM"
    <*> v .: "Value Check"
    <*> v .: "EBITDA"
    <*> v .: "Total Debt"
    <*> v .: "Free Cash Flow"
    <*> v .: "Gross Profit Margin"
    <*> v .: "Operating Margin"
    <*> v .: "Net Profit Margin"
    <*> v .: "Return on Equity"
    <*> v .: "Return on Assets"
    <*> v .: "Free Cash Flow to Net Income"
    <*> v .: "Current Ratio"
    <*> v .: "Liabilities to Equity Ratio"
    <*> v .: "Debt Ratio"
    <*> v .: "Earnings Per Share, Basic"
    <*> v .: "Earnings Per Share, Diluted"
    <*> v .: "Sales Per Share"
    <*> v .: "Equity Per Share"
    <*> v .: "Free Cash Flow Per Share"
    <*> v .: "Dividends Per Share"
    <*> v .: "Piotroski F-Score"
    <*> v .: "Return On Invested Capital"
    <*> v .: "Cash Return On Invested Capital"
    <*> v .: "Dividend Payout Ratio"
    <*> v .: "Net Debt / EBITDA"
    <*> v .: "Net Debt / EBIT"

newtype DerivedKeyed a = DerivedKeyed { unKeyDerived :: [DerivedRow a] }

instance (Read a, RealFrac a) => FromJSON (DerivedKeyed a) where
  parseJSON o = DerivedKeyed <$> (traverse parseJSON =<< createKeyedRows o)

fetchDerivedByTicker
  :: (Read a, RealFrac a, MonadThrow m, MonadIO m)
  => SimFinContext
  -> [Text]
  -> FiscalPeriod
  -> Int
  -> m [[DerivedRow a]]
fetchDerivedByTicker ctx tickers period year =
  fmap unKeyDerived <$> performRequest ctx "companies/statements"
    [ ("ticker", Just $ T.encodeUtf8 $ T.intercalate "," tickers)
    , ("statement", Just "derived")
    , ("period", Just $ fiscalPeriodParam period)
    , ("fyear", Just $ BS8.pack $ show year)
    ]

test :: IO ()
test = do
  ctx <- createDefaultContext
  -- print =<< fetchBalanceSheetsByTicker ctx ["CB"] Q1 2022
  -- print =<< fetchBalanceSheetsByTicker ctx ["C"] Q1 2022
  -- print =<< fetchBalanceSheetsByTicker ctx ["AAPL"] Q1 2022
  -- print =<< fetchProfitsAndLossesByTicker ctx ["GOOG"] FullYear 2020
  -- print =<< fetchProfitsAndLossesByTicker ctx ["C"] FullYear 2020
  -- print =<< fetchProfitsAndLossesByTicker ctx ["CB"] FullYear 2020
  -- print =<< fetchCashFlowsByTicker ctx ["GOOG"] FullYear 2020
  -- print =<< fetchCashFlowsByTicker ctx ["C"] FullYear 2020
  -- print =<< fetchCashFlowsByTicker ctx ["CB"] FullYear 2020
  -- print =<< fetchDerivedByTicker ctx ["AAPL"] FullYear 2020
  pure ()

