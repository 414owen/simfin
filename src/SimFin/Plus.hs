{-|
Module      : SimFin.Plus
Description : SimFin+ API.
Copyright   : (c) Owen Shepherd, 2022
License     : MIT
Maintainer  : owen@owen.cafe
-}

{-# LANGUAGE OverloadedStrings #-}

module SimFin.Plus
  ( SimFinContext(..)
  , Industry(..)

  , CompanyListingRow(..)
  , CompanyInfoRow(..)

  , GeneralBalanceSheetRow(..)
  , BankBalanceSheetRow(..)
  , InsuranceBalanceSheetRow(..)

  , GeneralProfitAndLossRow(..)
  , BankProfitAndLossRow(..)
  , InsuranceProfitAndLossRow(..)

  , GeneralCashFlowRow(..)
  , BankCashFlowRow(..)
  , InsuranceCashFlowRow(..)

  , DerivedRow(..)
  , PricesRow(..)
  , RatiosRow(..)

  , PricesAndRatiosRow(..)

  , PricesQuery(..)
  , StatementQuery(..)

  , StockRef(..)
  , FiscalPeriod(..)

  , ApiError(..)
  , ApiResult

  , createDefaultContext
  , fetchCompanyList

  , fetchCompanyInfo
  , fetchBalanceSheets
  , fetchProfitsAndLosses
  , fetchCashFlows
  , fetchDerived
  , fetchPrices
  , fetchPricesAndRatios
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Functor.Syntax
import Data.List.NonEmpty (NonEmpty)

import SimFin.Common
import SimFin.Internal
import SimFin.Types.BalanceSheet
import SimFin.Types.CompanyInfo
import SimFin.Types.CompanyListing
import SimFin.Types.CashFlow
import SimFin.Types.Derived
import SimFin.Types.FiscalPeriod
import SimFin.Types.Industry
import SimFin.Types.Prices
import SimFin.Types.PricesQuery
import SimFin.Types.PricesAndRatios
import SimFin.Types.ProfitAndLoss
import SimFin.Types.Ratios
import SimFin.Types.StatementQuery
import SimFin.Types.StockRef
import SimFin.Util

------
-- General Company Info
------

-- | Fetch general company information. See the [SimFin docs](https://simfin.com/api/v2/documentation/#tag/Company/paths/~1companies~1general/get).

fetchCompanyInfo
  :: (MonadThrow m, MonadIO m)
  => SimFinContext
  -> NonEmpty StockRef
  -> m (ApiResult [CompanyInfoRow])
fetchCompanyInfo ctx refs =
  performRequest ctx "companies/general" (stockRefsToQueryParams refs)

------
-- Balance Sheets
------

-- | Fetch a company's balance sheet statements.
-- | See the [SimFin docs](https://simfin.com/api/v2/documentation/#tag/Company/paths/~1companies~1statements/get).

fetchBalanceSheets
  :: (MonadThrow m, MonadIO m)
  => SimFinContext
  -> StatementQuery
  -> m (ApiResult [IndustryBalanceSheet])
fetchBalanceSheets ctx query =
  invertIndustries <$$> performRequest ctx "companies/statements"
    (("statement", Just "bs") : statementQueryToQueryParams query)

------
-- P&L
------

-- | Fetch a company's profit and loss statements.
-- | See the [SimFin docs](https://simfin.com/api/v2/documentation/#tag/Company/paths/~1companies~1statements/get).

fetchProfitsAndLosses
  :: (MonadThrow m, MonadIO m)
  => SimFinContext
  -> StatementQuery
  -> m (ApiResult [IndustryProfitAndLoss])
fetchProfitsAndLosses ctx query =
  invertIndustries <$$> performRequest ctx "companies/statements"
    (("statement", Just "pl") : statementQueryToQueryParams query)

-----
-- Cash Flows
------

-- | Fetch a company's cash flow statements.
-- | See the [SimFin docs](https://simfin.com/api/v2/documentation/#tag/Company/paths/~1companies~1statements/get).

fetchCashFlows
  :: (MonadThrow m, MonadIO m)
  => SimFinContext
  -> StatementQuery
  -> m (ApiResult [IndustryCashFlow])
fetchCashFlows ctx query =
  invertIndustries <$$> performRequest ctx "companies/statements"
    (("statement", Just "cf") : statementQueryToQueryParams query)

------
-- Derived
------

-- | Fetch a company's derived figures.
-- | See the [SimFin docs](https://simfin.com/api/v2/documentation/#tag/Company/paths/~1companies~1statements/get).

fetchDerived
  :: (Read a, RealFrac a, MonadThrow m, MonadIO m)
  => SimFinContext
  -> StatementQuery
  -> m (ApiResult [DerivedRow a])
fetchDerived ctx query =
  mconcat <$$> performRequest ctx "companies/statements"
    (("statement", Just "derived") : statementQueryToQueryParams query)

------
-- Prices
------

-- | Fetch a company's historical share prices. See the [SimFin docs](https://simfin.com/api/v2/documentation/#tag/Company/paths/~1companies~1prices/get).

fetchPrices
  :: (Read a, RealFrac a, MonadThrow m, MonadIO m)
  => SimFinContext
  -> PricesQuery
  -> m (ApiResult [PricesRow a])
fetchPrices ctx query =
  mconcat . fmap unKeyPrices <$$> performRequest ctx "companies/prices"
    (pricesQueryToQueryParams query)

-- | Fetch a company's historical share prices, along with key ratios. See the [SimFin docs](https://simfin.com/api/v2/documentation/#tag/Company/paths/~1companies~1prices/get).

fetchPricesAndRatios
  :: (Read a, RealFrac a, MonadThrow m, MonadIO m)
  => SimFinContext
  -> PricesQuery
  -> m (ApiResult [PricesAndRatiosRow a])
fetchPricesAndRatios ctx query =
  mconcat . fmap unKeyPricesAndRatios <$$> performRequest ctx "companies/prices"
    (("ratios", Nothing) : pricesQueryToQueryParams query)
