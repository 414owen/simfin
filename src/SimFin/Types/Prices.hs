{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module SimFin.Types.Prices
  ( PricesRow(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.Time.Calendar (Day)

import SimFin.Types.StringFrac

data PricesRow a
  = PricesRow
  { simFinId :: Int
  , ticker :: Text
  , date :: Maybe Day
  , open :: StringFrac a
  , high :: StringFrac a
  , low :: StringFrac a
  , close :: StringFrac a
  , adjClose :: StringFrac a
  , volume :: Integer
  , dividend :: Maybe (StringFrac a)
  , commonSharesOutstanding :: Maybe Integer
  } deriving Show

instance (Read a, RealFrac a) => FromJSON (PricesRow a) where
  parseJSON = withObject "PricesRow" $ \v -> PricesRow
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

