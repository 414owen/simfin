{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module SimFin
  ( SimFinContext(..)
  , FiscalPeriod(..)
  , GeneralBalanceSheetRow(..)
  , BankBalanceSheetRow(..)
  , InsuranceBalanceSheetRow(..)
  , Industry(..)
  , createDefaultContext
  , listCompanies
  , companyInformationById
  ) where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.UTF8 as BSU
import Data.Time.Calendar (Day)
import Data.Function ((&))
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

data Industry general bank insurance
  = General general
  | Bank bank
  | Insurance insurance
  deriving Show

instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON (Industry a b c) where
  parseJSON json = General <$> parseJSON json
    <|> Bank <$> parseJSON json
    <|> Insurance <$> parseJSON json

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

listCompanies :: (MonadThrow m, MonadIO m) => SimFinContext -> m CompanyListing
listCompanies ctx =
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

companyInformationById :: (MonadThrow m, MonadIO m) => SimFinContext -> [Int] -> m [CompanyInformation]
companyInformationById ctx ids =
  fmap unKeyCompanyInfo <$> performRequest ctx "companies/general"
    [ ("id", Just $ BS8.intercalate "," $ BS8.pack . show <$> ids) ]

companyInformationByTicker :: (MonadThrow m, MonadIO m) => SimFinContext -> [Text] -> m [CompanyInformation]
companyInformationByTicker ctx tickers =
  fmap unKeyCompanyInfo <$> performRequest ctx "companies/general"
    [ ("ticker", Just $ T.encodeUtf8 $ T.intercalate "," tickers) ]

------
-- Fundamentals
------

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
  , investmentsInAffiliates :: Maybe Integer
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
  , currentPortionofLongTermDebt :: Maybe Integer
  , otherShortTermLiabilities :: Maybe Integer
  , deferredRevenueShortTerm :: Maybe Integer
  , liabilitiesFromDerivativesAndHedgingShortTerm :: Maybe Integer
  , deferredTaxLiabilitiesShortTerm :: Maybe Integer
  , liabilitiesFromDiscontinuedOperationsShortTerm :: Maybe Integer
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
  , liabilitiesFromDerivativesAndHedgingLongTerm :: Maybe Integer
  , liabilitiesFromDiscontinuedOperationsLongTerm :: Maybe Integer
  , miscLongTermLiabilities :: Maybe Integer
  , totalNoncurrentLiabilities :: Maybe Integer
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

instance FromJSON GeneralBalanceSheetRow where
  parseJSON = withObject "GeneralBalanceSheet" $ \v -> GeneralBalanceSheetRow
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
  parseJSON = withObject "BankBalanceSheet" $ \v -> BankBalanceSheetRow
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
  parseJSON = withObject "InsuranceBalanceSheet" $ \v -> InsuranceBalanceSheetRow
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

-- TODO abstract
unKeyIndustryBalanceSheets :: IndustryBalanceSheetsKeyed -> IndustryBalanceSheets
unKeyIndustryBalanceSheets a = case a of
  General general -> General $ unKeyGeneralBalanceSheets general
  Bank bank -> Bank $ unKeyBankBalanceSheets bank
  Insurance insurance -> Insurance $ unKeyInsuranceBalanceSheets insurance

generalBalanceSheetByTicker :: (MonadThrow m, MonadIO m) => SimFinContext -> [Text] -> FiscalPeriod -> Int -> m [IndustryBalanceSheets]
generalBalanceSheetByTicker ctx tickers period year =
  fmap unKeyIndustryBalanceSheets <$> performRequest ctx "companies/statements"
    [ ("ticker", Just $ T.encodeUtf8 $ T.intercalate "," tickers)
    , ("statement", Just $ "bs")
    , ("period", Just $ fiscalPeriodParam period)
    , ("fyear", Just $ BS8.pack $ show year)
    ]

test :: IO ()
test = do
  ctx <- createDefaultContext
  -- print =<< generalBalanceSheetByTicker ctx ["AAPL"] Q1 2022
  -- print =<< generalBalanceSheetByTicker ctx ["C"] Q1 2022
  print =<< generalBalanceSheetByTicker ctx ["GIECO"] Q1 2022
  pure ()
