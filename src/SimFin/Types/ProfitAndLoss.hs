{-|
Module      : SimFin.Types.ProfitAndLoss
Description : Types to represent a company's profit and loss statement.
Copyright   : (c) Owen Shepherd, 2022
License     : MIT
Maintainer  : owen@owen.cafe
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module SimFin.Types.ProfitAndLoss
  ( GeneralProfitAndLossRow(..)
  , BankProfitAndLossRow(..)
  , InsuranceProfitAndLossRow(..)
  , IndustryProfitsAndLosses
  , IndustryProfitAndLoss
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

-- | Cash flow statement for general companies. 

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

-- | Wrapper to parse a GeneralProfitAndLossRow record from SimFin's JSON format.

newtype GeneralProfitsAndLossesKeyed = GeneralProfitsAndLossesKeyed { unKeyGeneralProfitsAndLosses :: [GeneralProfitAndLossRow] }

instance FromJSON GeneralProfitsAndLossesKeyed where
  parseJSON o = GeneralProfitsAndLossesKeyed <$> (traverse parseJSON =<< createKeyedRows o)


------
-- Bank
------

-- | Cash flow statement for banks. 

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

-- | Wrapper to parse a BankProfitAndLossRow record from SimFin's JSON format.

newtype BankProfitsAndLossesKeyed = BankProfitsAndLossesKeyed { unKeyBankProfitsAndLosses :: [BankProfitAndLossRow] }

instance FromJSON BankProfitsAndLossesKeyed where
  parseJSON o = BankProfitsAndLossesKeyed <$> (traverse parseJSON =<< createKeyedRows o)


------
-- Insurance
------

-- | Cash flow statement for insurance companies. 

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

-- | Wrapper to parse an InsuranceProfitAndLossRow record from SimFin's JSON format.

newtype InsuranceProfitsAndLossesKeyed = InsuranceProfitsAndLossesKeyed { unKeyInsuranceProfitsAndLosses :: [InsuranceProfitAndLossRow] }

instance FromJSON InsuranceProfitsAndLossesKeyed where
  parseJSON o = InsuranceProfitsAndLossesKeyed <$> (traverse parseJSON =<< createKeyedRows o)

------
-- Industry
------

type IndustryProfitsAndLossesKeyed
  = Industry GeneralProfitsAndLossesKeyed BankProfitsAndLossesKeyed InsuranceProfitsAndLossesKeyed

-- | Discrimination of profit and loss lists.

type IndustryProfitsAndLosses
  = Industry [GeneralProfitAndLossRow] [BankProfitAndLossRow] [InsuranceProfitAndLossRow]

-- | Discrimination of profit and losses.

type IndustryProfitAndLoss
  = Industry GeneralProfitAndLossRow BankProfitAndLossRow InsuranceProfitAndLossRow

instance FromJSON IndustryProfitsAndLossesKeyed where
  parseJSON root = General <$> parseJSON root
    <|> Insurance <$> parseJSON root
    <|> Bank <$> parseJSON root

unKeyIndustryProfitsAndLosses :: IndustryProfitsAndLossesKeyed -> IndustryProfitsAndLosses
unKeyIndustryProfitsAndLosses = mapIndustry
  unKeyGeneralProfitsAndLosses
  unKeyBankProfitsAndLosses
  unKeyInsuranceProfitsAndLosses

instance FromJSON IndustryProfitsAndLosses where
  parseJSON root = unKeyIndustryProfitsAndLosses <$> parseJSON root
