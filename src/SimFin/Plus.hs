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

------
-- General Company Info
------

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

fetchPrices
  :: (Read a, RealFrac a, MonadThrow m, MonadIO m)
  => SimFinContext
  -> PricesQuery
  -> m (ApiResult [PricesRow a])
fetchPrices ctx query =
  mconcat . fmap unKeyPrices <$$> performRequest ctx "companies/prices"
    (pricesQueryToQueryParams query)

fetchPricesAndRatios
  :: (Read a, RealFrac a, MonadThrow m, MonadIO m)
  => SimFinContext
  -> PricesQuery
  -> m (ApiResult [PricesAndRatiosRow a])
fetchPricesAndRatios ctx query =
  mconcat . fmap unKeyPricesAndRatios <$$> performRequest ctx "companies/prices"
    (("ratios", Nothing) : pricesQueryToQueryParams query)
