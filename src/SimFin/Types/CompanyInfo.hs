{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module SimFin.Types.CompanyInfo
  ( CompanyInfoRow(..)
  ) where

import Control.Monad ((>=>))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)

import SimFin.Util

data CompanyInfoRow
  = CompanyInfoRow
  { simFinId :: Int
  , ticker :: Text
  , companyName :: Text
  , industryId :: Int
  , monthFYEnd :: Int
  , numberEmployees :: Int
  , businessSummary :: Text
  } deriving Show

instance FromJSON CompanyInfoRow where
  parseJSON = createKeyedRow >=> withObject "CompanyInfoRow" f
    where
      f :: Object -> Parser CompanyInfoRow
      f = \v -> CompanyInfoRow
        <$> v .: "SimFinId"
        <*> v .: "Ticker"
        <*> v .: "Company Name"
        <*> v .: "IndustryId"
        <*> v .: "Month FY End"
        <*> v .: "Number Employees"
        <*> v .: "Business Summary"
