{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module SimFin.Types.Derived
  ( DerivedRow(..)
  , DerivedRowsKeyed(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.Time.Calendar (Day)

import SimFin.Types.StringFrac
import SimFin.Types.FiscalPeriod
import SimFin.Internal

data DerivedRow a
  = DerivedRow
  { simFinId :: Int
  , ticker :: Text
  , fiscalPeriod :: FiscalPeriod
  , fiscalYear :: Int
  , reportDate :: Day
  , publishDate :: Day
  , restatedDate :: Day
  , source :: Text
  , tTM :: Bool
  , valueCheck :: Bool
  , eBITDA :: Maybe a
  , totalDebt :: Maybe a
  , freeCashFlow :: Maybe a
  , grossProfitMargin :: Maybe a
  , operatingMargin :: Maybe a
  , netProfitMargin :: Maybe a
  , returnOnEquity :: Maybe a
  , returnOnAssets :: Maybe a
  , freeCashFlowToNetIncome :: Maybe a
  , currentRatio :: Maybe a
  , liabilitiesToEquityRatio :: Maybe a
  , debtRatio :: Maybe a
  , earningsPerShareBasic :: Maybe a
  , earningsPerShareDiluted :: Maybe a
  , salesPerShare :: Maybe a
  , equityPerShare :: Maybe a
  , freeCashFlowPerShare :: Maybe a
  , dividendsPerShare :: Maybe a
  , piotroskiFScore :: Maybe Int
  , returnOnInvestedCapital :: Maybe a
  , cashReturnOnInvestedCapital :: Maybe a
  , dividendPayoutRatio :: Maybe a
  , netDebtEBITDA :: Maybe a
  , netDebtEBIT :: Maybe a
  } deriving (Functor, Show)

instance (Read a, RealFrac a) => FromJSON (DerivedRow a) where
  parseJSON = withObject "DerivedRow" $ \v -> fmap (fmap unStringFrac) $ DerivedRow
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

newtype DerivedRowsKeyed a = DerivedRowsKeyed { unDerivedRows :: [DerivedRow a] }

instance (Read a, RealFrac a) => FromJSON (DerivedRowsKeyed a) where
  parseJSON o = fmap DerivedRowsKeyed $ traverse parseJSON =<< createKeyedRows o
