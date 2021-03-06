{-|
Module      : SimFin.Types.Ratios
Description : Ratios derived from a business' financial statements.
Copyright   : (c) Owen Shepherd, 2022
License     : MIT
Maintainer  : owen@owen.cafe
-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module SimFin.Types.Ratios
  ( RatiosRow(..)
  ) where

import Data.Aeson

import SimFin.Types.StringFrac

-- | Record modelling the extra data returned by calling the share price API endpoint with the "&ratios"
-- query parameter. See the [SimFin docs](https://simfin.com/api/v2/documentation/#tag/Company/paths/~1companies~1prices/get).

data RatiosRow a
  = RatiosRow
  { marketCap :: Integer
  , priceToEarningsRatioQuarterly :: Maybe a
  , priceToEarningsRatioTTM :: Maybe a
  , priceToSalesRatioQuarterly :: Maybe a
  , priceToSalesRatioTTM :: Maybe a
  , priceToBookValueTTM :: Maybe a
  , priceToFreeCashFlowQuarterly :: Maybe a
  , priceToFreeCashFlowTTM :: Maybe a
  , enterpriseValueTTM :: Maybe a
  , eVEBITDATTM :: Maybe a
  , eVSalesTTM :: Maybe a
  , eVFCFTTM :: Maybe a
  , bookToMarketValueTTM :: Maybe a
  , operatingIncomeEVTTM :: Maybe a
  } deriving (Functor, Show)

instance (Read a, RealFrac a) => FromJSON (RatiosRow a) where
  parseJSON = withObject "RatiosRow" $ \v -> fmap (fmap unStringFrac) $ RatiosRow
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
