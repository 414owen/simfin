{-|
Module      : SimFin.Types.Prices
Description : Types to represent SimFin price results.
Copyright   : (c) Owen Shepherd, 2022
License     : MIT
Maintainer  : owen@owen.cafe
-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module SimFin.Types.Prices
  ( PricesRow(..)
  , PricesKeyed(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.Time.Calendar (Day)

import SimFin.Types.StringFrac
import SimFin.Internal

-- | Prices of a company over a single day. 

data PricesRow a
  = PricesRow
  { simFinId :: Int
  , ticker :: Text
  , date :: Maybe Day
  , open :: a
  , high :: a
  , low :: a
  , close :: a
  , adjClose :: a
  , volume :: Integer
  , dividend :: Maybe a
  , commonSharesOutstanding :: Maybe Integer
  } deriving (Functor, Show)

instance (Read a, RealFrac a) => FromJSON (PricesRow a) where
  parseJSON = withObject "PricesRow" $ \v -> fmap (fmap unStringFrac) $ PricesRow
    <$> v .: "SimFinId"
    <*> v .: "Ticker"
    <*> v .: "Date"
    <*> v .: "Open"
    <*> v .: "High"
    <*> v .: "Low"
    <*> v .: "Close"
    <*> v .: "Adj. Close"
    <*> v .: "Volume"
    <*> v .: "Dividend"
    <*> v .: "Common Shares Outstanding"

-- | Wrapper to parse a PricesRow record from SimFin's JSON format.

newtype PricesKeyed a = PricesKeyed { unKeyPrices :: [PricesRow a] }

instance (Read a, RealFrac a) => FromJSON (PricesKeyed a) where
  parseJSON o = PricesKeyed <$> (traverse parseJSON =<< createKeyedRows o)
