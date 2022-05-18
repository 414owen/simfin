{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module SimFin.Types.Derived
  ( DerivedRow(..)
  , DerivedRows(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.Time.Calendar (Day)

import SimFin.Types.StringFrac
import SimFin.Util

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

newtype DerivedRows a = DerivedRows { unDerivedRows :: [DerivedRow a] }

instance (Read a, RealFrac a) => FromJSON (DerivedRows a) where
  parseJSON o = fmap DerivedRows $ traverse parseJSON =<< createKeyedRows o
