{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module SimFin.Types.CashFlow
  ( GeneralCashFlowRow(..)
  , BankCashFlowRow(..)
  , InsuranceCashFlowRow(..)
  , IndustryCashFlows
  , IndustryCashFlow
  ) where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Text (Text)
import Data.Time.Calendar (Day)

import SimFin.Types.Industry
import SimFin.Internal


------
-- General
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
  , otherAdjustmensts :: Maybe Integer
  , depreciationAndAmortizatison :: Maybe Integer
  , nonCashItems :: Maybe Integer
  , stockBasedCompensatison :: Maybe Integer
  , deferredIncomeTaxes :: Maybe Integer
  , otherNonCashAdjustments :: Maybe Integer
  , changeInWorkingCapital :: Maybe Integer
  , changeInAccountsReceivable :: Maybe Integer
  , changeInInventories :: Maybe Integer
  , changeInAccountsPayable :: Maybe Integer
  , changeInOther :: Maybe Integer
  , netCashFromDiscontinuedOperationsOperating :: Maybe Integer
  , netCashFromOpesratingActivities :: Maybe Integer
  , changeInFixedAssetsAndIntsangibles :: Maybe Integer
  , dispositionOfFixedAssetssAndIntangibles :: Maybe Integer
  , dispositionOfFixedAsssets :: Maybe Integer
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

newtype GeneralCashFlowsKeyed = GeneralCashFlowsKeyed { unKeyGeneralCashFlows :: [GeneralCashFlowRow] }

instance FromJSON GeneralCashFlowsKeyed where
  parseJSON o = GeneralCashFlowsKeyed <$> (traverse parseJSON =<< createKeyedRows o)


------
-- Bank
------

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

newtype BankCashFlowsKeyed = BankCashFlowsKeyed { unKeyBankCashFlows :: [BankCashFlowRow] }

instance FromJSON BankCashFlowsKeyed where
  parseJSON o = BankCashFlowsKeyed <$> (traverse parseJSON =<< createKeyedRows o)


------
-- Insurance
------

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

newtype InsuranceCashFlowsKeyed = InsuranceCashFlowsKeyed { unKeyInsuranceCashFlows :: [InsuranceCashFlowRow] }

instance FromJSON InsuranceCashFlowsKeyed where
  parseJSON o = InsuranceCashFlowsKeyed <$> (traverse parseJSON =<< createKeyedRows o)


------
-- Industry
------

type IndustryCashFlowsKeyed
  = Industry GeneralCashFlowsKeyed BankCashFlowsKeyed InsuranceCashFlowsKeyed

type IndustryCashFlows
  = Industry [GeneralCashFlowRow] [BankCashFlowRow] [InsuranceCashFlowRow]

type IndustryCashFlow
  = Industry GeneralCashFlowRow BankCashFlowRow InsuranceCashFlowRow

instance FromJSON IndustryCashFlowsKeyed where
  parseJSON root = Insurance <$> parseJSON root
    <|> Bank <$> parseJSON root
    <|> General <$> parseJSON root

unKeyIndustryCashFlows :: IndustryCashFlowsKeyed -> IndustryCashFlows
unKeyIndustryCashFlows = mapIndustry
  unKeyGeneralCashFlows
  unKeyBankCashFlows
  unKeyInsuranceCashFlows

instance FromJSON IndustryCashFlows where
  parseJSON root = unKeyIndustryCashFlows <$> parseJSON root
