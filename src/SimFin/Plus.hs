{-# LANGUAGE OverloadedStrings #-}

module SimFin.Plus
  ( SimFinContext(..)
  , Industry(..)

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
import Data.List.NonEmpty (NonEmpty)

import SimFin.Internal
import SimFin.Types.BalanceSheet
import SimFin.Types.CompanyInfo
import SimFin.Types.CompanyListing
import SimFin.Types.CashFlow
import SimFin.Types.Derived
import SimFin.Types.Industry
import SimFin.Types.Prices
import SimFin.Types.PricesQuery
import SimFin.Types.PricesAndRatios
import SimFin.Types.ProfitAndLoss
import SimFin.Types.Ratios
import SimFin.Types.StatementQuery
import SimFin.Types.StockRef

------
-- List companies
------

fetchCompanyList :: (MonadThrow m, MonadIO m) => SimFinContext -> m [CompanyListingRow]
fetchCompanyList ctx =
  unKeyCompanyListing <$> performRequest ctx "companies/list" []

------
-- General Company Info
------

fetchCompanyInfo
  :: (MonadThrow m, MonadIO m)
  => SimFinContext
  -> NonEmpty StockRef
  -> m [CompanyInfoRow]
fetchCompanyInfo ctx refs =
  performRequest ctx "companies/general" (stockRefsToQueryParams refs)

------
-- Balance Sheets
------

fetchBalanceSheets
  :: (MonadThrow m, MonadIO m)
  => SimFinContext
  -> StatementQuery
  -> m [IndustryBalanceSheets]
fetchBalanceSheets ctx query = performRequest ctx "companies/statements"
  (("statement", Just "bs") : statementQueryToQueryParams query)

------
-- P&L
------

fetchProfitsAndLosses
  :: (MonadThrow m, MonadIO m)
  => SimFinContext
  -> StatementQuery
  -> m [IndustryProfitsAndLosses]
fetchProfitsAndLosses ctx query = performRequest ctx "companies/statements"
  (("statement", Just "pl") : statementQueryToQueryParams query)

-----
-- Cash Flows
------

fetchCashFlows
  :: (MonadThrow m, MonadIO m)
  => SimFinContext
  -> StatementQuery
  -> m [IndustryCashFlows]
fetchCashFlows ctx query = performRequest ctx "companies/statements"
  (("statement", Just "cf") : statementQueryToQueryParams query)

------
-- Derived
------

fetchDerived
  :: (Read a, RealFrac a, MonadThrow m, MonadIO m)
  => SimFinContext
  -> StatementQuery
  -> m [DerivedRow a]
fetchDerived ctx query =
  mconcat <$> performRequest ctx "companies/statements"
    (("statement", Just "derived") : statementQueryToQueryParams query)

------
-- Prices
------

fetchPrices
  :: (Read a, RealFrac a, MonadThrow m, MonadIO m)
  => SimFinContext
  -> PricesQuery
  -> m [PricesRow a]
fetchPrices ctx query =
  mconcat . fmap unKeyPrices <$> performRequest ctx "companies/prices"
    (pricesQueryToQueryParams query)

fetchPricesAndRatios
  :: (Read a, RealFrac a, MonadThrow m, MonadIO m)
  => SimFinContext
  -> PricesQuery
  -> m [PricesAndRatiosRow a]
fetchPricesAndRatios ctx query =
  mconcat . fmap unKeyPricesAndRatios <$> performRequest ctx "companies/prices"
    (("ratios", Nothing) : pricesQueryToQueryParams query)
