{-|
Module      : SimFin.Types.CompanyInfo
Description : General information about a company.
Copyright   : (c) Owen Shepherd, 2022
License     : MIT
Maintainer  : owen@owen.cafe
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module SimFin.Types.CompanyInfo
  ( CompanyInfoRow(..)
  ) where

import Control.Monad ((>=>))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)

import SimFin.Internal

-- | Genreal information about a company. See the [SimFin docs](https://simfin.com/api/v2/documentation/#tag/Company/paths/~1companies~1general/get).

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
