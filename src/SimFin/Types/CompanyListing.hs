{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module SimFin.Types.CompanyListing
  ( CompanyListingRow(..)
  , CompanyListingKeyed(..)
  ) where

import Data.Aeson
import Data.Text (Text)

import SimFin.Util

data CompanyListingRow
  = CompanyListingRow
  { simFinId :: Int
  , ticker :: Text
  } deriving Show

newtype CompanyListingKeyed = CompanyListingKeyed { unKeyCompanyListing :: [CompanyListingRow] }

instance FromJSON CompanyListingRow where
  parseJSON = withObject "CompanyListing" $ \v -> CompanyListingRow
    <$> v .: "SimFinId"
    <*> v .: "Ticker"

instance FromJSON CompanyListingKeyed where
  parseJSON o = CompanyListingKeyed <$> (traverse parseJSON =<< createKeyedRows o)
