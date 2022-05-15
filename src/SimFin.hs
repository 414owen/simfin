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


-- Proofs that this parsing order works for all cases:
-- in P&L, |General| > |Insurance| > |Bank|
-- in Balance Sheets, |General| > |Bank| > |Insurance|, and Insurance has "Life Policy benefits"
instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON (Industry a b c) where
  parseJSON json = General <$> parseJSON json
    <|> Insurance <$> parseJSON json
    <|> Bank <$> parseJSON json

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

unKeyIndustryBalanceSheets :: IndustryBalanceSheetsKeyed -> IndustryBalanceSheets
unKeyIndustryBalanceSheets = mapIndustry
  unKeyGeneralBalanceSheets
  unKeyBankBalanceSheets
  unKeyInsuranceBalanceSheets

balanceSheetsByTicker :: (MonadThrow m, MonadIO m) => SimFinContext -> [Text] -> FiscalPeriod -> Int -> m [IndustryBalanceSheets]
balanceSheetsByTicker ctx tickers period year =
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

newtype GeneralProfitAndLossesKeyed = GeneralProfitAndLossesKeyed { unKeyGeneralProfitAndLosses :: [GeneralProfitAndLossRow] }

instance FromJSON GeneralProfitAndLossesKeyed where
  parseJSON o = GeneralProfitAndLossesKeyed <$> (traverse parseJSON =<< createKeyedRows o)

newtype BankProfitAndLossesKeyed = BankProfitAndLossesKeyed { unKeyBankProfitAndLosses :: [BankProfitAndLossRow] }

instance FromJSON BankProfitAndLossesKeyed where
  parseJSON o = BankProfitAndLossesKeyed <$> (traverse parseJSON =<< createKeyedRows o)

newtype InsuranceProfitAndLossesKeyed = InsuranceProfitAndLossesKeyed { unKeyInsuranceProfitAndLosses :: [InsuranceProfitAndLossRow] }

instance FromJSON InsuranceProfitAndLossesKeyed where
  parseJSON o = InsuranceProfitAndLossesKeyed <$> (traverse parseJSON =<< createKeyedRows o)

type IndustryProfitAndLossesKeyed
  = Industry GeneralProfitAndLossesKeyed BankProfitAndLossesKeyed InsuranceProfitAndLossesKeyed

type IndustryProfitAndLosses
  = Industry [GeneralProfitAndLossRow] [BankProfitAndLossRow] [InsuranceProfitAndLossRow]

unKeyIndustryProfitAndLosses :: IndustryProfitAndLossesKeyed -> IndustryProfitAndLosses
unKeyIndustryProfitAndLosses = mapIndustry
  unKeyGeneralProfitAndLosses
  unKeyBankProfitAndLosses
  unKeyInsuranceProfitAndLosses

profitAndLossesByTicker :: (MonadThrow m, MonadIO m) => SimFinContext -> [Text] -> FiscalPeriod -> Int -> m [IndustryProfitAndLosses]
profitAndLossesByTicker ctx tickers period year =
  fmap unKeyIndustryProfitAndLosses <$> performRequest ctx "companies/statements"
    [ ("ticker", Just $ T.encodeUtf8 $ T.intercalate "," tickers)
    , ("statement", Just "pl")
    , ("period", Just $ fiscalPeriodParam period)
    , ("fyear", Just $ BS8.pack $ show year)
    ]

test :: IO ()
test = do
  ctx <- createDefaultContext
  -- print =<< generalBalanceSheetByTicker ctx ["AAPL"] Q1 2022
  -- print =<< generalBalanceSheetByTicker ctx ["C"] Q1 2022
  -- print =<< balanceSheetsByTicker ctx ["CB"] Q1 2022
  -- print =<< profitAndLossesByTicker ctx ["GOOG"] FullYear 2020
  -- print =<< profitAndLossesByTicker ctx ["C"] FullYear 2020
  -- print =<< profitAndLossesByTicker ctx ["CB"] FullYear 2020
  pure ()
