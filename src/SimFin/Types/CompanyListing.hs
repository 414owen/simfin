{-|
Module      : SimFin.Types.CompanyListing
Description : Item of a list of company tickers and their SimFin IDs.
Copyright   : (c) Owen Shepherd, 2022
License     : MIT
Maintainer  : owen@owen.cafe
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module SimFin.Types.CompanyListing
  ( CompanyListingRow(..)
  , CompanyListingKeyed(..)
  ) where

import Data.Aeson
import Data.Text (Text)

import SimFin.Internal

-- | SimFin ID and company ticker. See the [SimFin docs](https://simfin.com/api/v2/documentation/#tag/Company/paths/~1companies~1list/get).

data CompanyListingRow
  = CompanyListingRow
  { simFinId :: Int
  , ticker :: Text
  } deriving Show

-- | Wrapper to parse a CompanyListing record from SimFin's JSON format.
-- You probably don't want to use this.

newtype CompanyListingKeyed = CompanyListingKeyed { unKeyCompanyListing :: [CompanyListingRow] }

instance FromJSON CompanyListingRow where
  parseJSON = withObject "CompanyListing" $ \v -> CompanyListingRow
    <$> v .: "SimFinId"
    <*> v .: "Ticker"

instance FromJSON CompanyListingKeyed where
  parseJSON o = CompanyListingKeyed <$> (traverse parseJSON =<< createKeyedRows o)
