{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module SimFin.Types.Ratios
  ( RatiosRow(..)
  ) where

import Data.Aeson

import SimFin.Types.StringFrac

data RatiosRow a
  = RatiosRow
  { marketCap :: Integer
  , priceToEarningsRatioQuarterly :: Maybe (StringFrac a)
  , priceToEarningsRatioTTM :: Maybe (StringFrac a)
  , priceToSalesRatioQuarterly :: Maybe (StringFrac a)
  , priceToSalesRatioTTM :: Maybe (StringFrac a)
  , priceToBookValueTTM :: Maybe (StringFrac a)
  , priceToFreeCashFlowQuarterly :: Maybe (StringFrac a)
  , priceToFreeCashFlowTTM :: Maybe (StringFrac a)
  , enterpriseValueTTM :: Maybe (StringFrac a)
  , eVEBITDATTM :: Maybe (StringFrac a)
  , eVSalesTTM :: Maybe (StringFrac a)
  , eVFCFTTM :: Maybe (StringFrac a)
  , bookToMarketValueTTM :: Maybe (StringFrac a)
  , operatingIncomeEVTTM :: Maybe (StringFrac a)
  } deriving Show

instance (Read a, RealFrac a) => FromJSON (RatiosRow a) where
  parseJSON = withObject "RatiosRow" $ \v -> RatiosRow
    <$> v .: "Market-Cap"
    <*> v .: "Price to Earnings Ratio (quarterly)"
    <*> v .: "Price to Earnings Ratio (ttm)"
    <*> v .: "Price to Sales Ratio (quarterly)"
    <*> v .: "Price to Sales Ratio (ttm)"
    <*> v .: "Price to Book Value (ttm)"
    <*> v .: "Price to Free Cash Flow (quarterly)"
    <*> v .: "Price to Free Cash Flow (ttm)"
    <*> v .: "Enterprise Value (ttm)"
    <*> v .: "EV/EBITDA (ttm)"
    <*> v .: "EV/Sales (ttm)"
    <*> v .: "EV/FCF (ttm)"
    <*> v .: "Book to Market Value (ttm)"
    <*> v .: "Operating Income/EV (ttm)"
