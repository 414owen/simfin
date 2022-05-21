{-|
Module      : SimFin.Free
Description : SimFin Free API.
Copyright   : (c) Owen Shepherd, 2022
License     : MIT
Maintainer  : owen@owen.cafe
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SimFin.Free
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

  , PricesQueryFree
  , StatementQueryFree(..)

  , StockRef(..)
  , FiscalPeriod(..)

  , ApiError(..)
  , ApiResult

  , createDefaultContext
  , fetchCompanyList

  , fetchCompanyInfo
  , fetchBalanceSheet
  , fetchProfitAndLoss
  , fetchCashFlow
  , fetchDerived
  , fetchPrices
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Functor.Syntax
import Data.Maybe (listToMaybe)

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
import SimFin.Types.ProfitAndLoss
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
  -> StockRef
  -> m (ApiResult (Maybe CompanyInfoRow))
fetchCompanyInfo ctx refs = do
  rows :: ApiResult [CompanyInfoRow] <- performRequest ctx "companies/general"
    $ stockRefsToQueryParams $ pure refs
  pure $ listToMaybe <$> rows

------
-- Balance Sheets
------

-- | Fetch a company's balance sheet statement. As this is the free API version, only one statement
-- is returned. The returned statement's data is dependent on the company type.
-- See the [SimFin docs](https://simfin.com/api/v2/documentation/#tag/Company/paths/~1companies~1statements/get).

fetchBalanceSheet
  :: (MonadThrow m, MonadIO m)
  => SimFinContext
  -> StatementQueryFree
  -> m (ApiResult (Maybe IndustryBalanceSheet))
fetchBalanceSheet ctx query = do
  nested :: ApiResult [IndustryBalanceSheets] <- performRequest ctx "companies/statements"
    $ ("statement", Just "bs") : statementQueryFreeToQueryParams query
  pure $ listToMaybe . invertIndustries <$> nested

------
-- P&L
------

-- | Fetch a company's profit and loss statement. As this is the free API version, only one statement
-- is returned. The returned statement's data is dependent on the company type.
-- See the [SimFin docs](https://simfin.com/api/v2/documentation/#tag/Company/paths/~1companies~1statements/get).

fetchProfitAndLoss
  :: (MonadThrow m, MonadIO m)
  => SimFinContext
  -> StatementQueryFree
  -> m (ApiResult (Maybe IndustryProfitAndLoss))
fetchProfitAndLoss ctx query = do
  nested :: ApiResult [IndustryProfitsAndLosses] <- performRequest ctx "companies/statements"
    $ ("statement", Just "pl") : statementQueryFreeToQueryParams query
  pure $ listToMaybe . invertIndustries <$> nested

-----
-- Cash Flows
------

-- | Fetch a company's cash flow statement. As this is the free API version, only one statement
-- is returned. The returned statement's data is dependent on the company type.
-- See the [SimFin docs](https://simfin.com/api/v2/documentation/#tag/Company/paths/~1companies~1statements/get).

fetchCashFlow
  :: (MonadThrow m, MonadIO m)
  => SimFinContext
  -> StatementQueryFree
  -> m (ApiResult (Maybe IndustryCashFlow))
fetchCashFlow ctx query = do
  nested :: ApiResult [IndustryCashFlows] <- performRequest ctx "companies/statements"
    $ ("statement", Just "cf") : statementQueryFreeToQueryParams query
  pure $ listToMaybe . invertIndustries <$> nested

------
-- Derived
------

-- | Fetch a company's derived figures. As this is the free API version, only one set
-- of data is returned.
-- See the [SimFin docs](https://simfin.com/api/v2/documentation/#tag/Company/paths/~1companies~1statements/get).

fetchDerived
  :: forall m a. (Read a, RealFrac a, MonadThrow m, MonadIO m)
  => SimFinContext
  -> StatementQueryFree
  -> m (ApiResult (Maybe (DerivedRow a)))
fetchDerived ctx query = do
  nested :: ApiResult [DerivedRowsKeyed a] <- performRequest ctx "companies/statements"
    (("statement", Just "derived") : statementQueryFreeToQueryParams query)
  pure $ listToMaybe . mconcat . fmap unDerivedRows <$> nested

------
-- Prices
------

-- | Fetch a company's historical share prices. See the [SimFin docs](https://simfin.com/api/v2/documentation/#tag/Company/paths/~1companies~1prices/get).

fetchPrices
  :: (Read a, RealFrac a, MonadThrow m, MonadIO m)
  => SimFinContext
  -> PricesQueryFree
  -> m (ApiResult [PricesRow a])
fetchPrices ctx query =
  mconcat . fmap unKeyPrices <$$> performRequest ctx "companies/prices"
    (pricesQueryFreeToQueryParams query)
