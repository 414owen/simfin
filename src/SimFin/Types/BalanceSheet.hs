{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module SimFin.Types.BalanceSheet
  ( GeneralBalanceSheetRow(..)
  , BankBalanceSheetRow(..)
  , InsuranceBalanceSheetRow(..)
  , IndustryBalanceSheets
  ) where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Text (Text)
import Data.Time.Calendar (Day)

import SimFin.Types.Industry
import SimFin.Util

------
-- General
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

newtype GeneralBalanceSheetsKeyed = GeneralBalanceSheetsKeyed { unKeyGeneralBalanceSheets :: [GeneralBalanceSheetRow] }

instance FromJSON GeneralBalanceSheetsKeyed where
  parseJSON o = GeneralBalanceSheetsKeyed <$> (traverse parseJSON =<< createKeyedRows o)


------
-- Bank
------

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

newtype BankBalanceSheetsKeyed = BankBalanceSheetsKeyed { unKeyBankBalanceSheets :: [BankBalanceSheetRow] }

instance FromJSON BankBalanceSheetsKeyed where
  parseJSON o = BankBalanceSheetsKeyed <$> (traverse parseJSON =<< createKeyedRows o)


------
-- Insurance
------

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

newtype InsuranceBalanceSheetsKeyed = InsuranceBalanceSheetsKeyed { unKeyInsuranceBalanceSheets :: [InsuranceBalanceSheetRow] }

instance FromJSON InsuranceBalanceSheetsKeyed where
  parseJSON o = InsuranceBalanceSheetsKeyed <$> (traverse parseJSON =<< createKeyedRows o)


------
-- Industry
------

type IndustryBalanceSheetsKeyed
  = Industry GeneralBalanceSheetsKeyed BankBalanceSheetsKeyed InsuranceBalanceSheetsKeyed

type IndustryBalanceSheets
  = Industry [GeneralBalanceSheetRow] [BankBalanceSheetRow] [InsuranceBalanceSheetRow]

instance FromJSON IndustryBalanceSheetsKeyed where
  parseJSON json = General <$> parseJSON json
    <|> Bank <$> parseJSON json
    <|> Insurance <$> parseJSON json

unKeyIndustryBalanceSheets :: IndustryBalanceSheetsKeyed -> IndustryBalanceSheets
unKeyIndustryBalanceSheets = mapIndustry
  unKeyGeneralBalanceSheets
  unKeyBankBalanceSheets
  unKeyInsuranceBalanceSheets

instance FromJSON IndustryBalanceSheets where
  parseJSON json = unKeyIndustryBalanceSheets <$> parseJSON json
