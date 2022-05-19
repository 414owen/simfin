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


------
-- General Company Info
------

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

fetchPrices
  :: (Read a, RealFrac a, MonadThrow m, MonadIO m)
  => SimFinContext
  -> PricesQueryFree
  -> m (ApiResult [PricesRow a])
fetchPrices ctx query =
  mconcat . fmap unKeyPrices <$$> performRequest ctx "companies/prices"
    (pricesQueryFreeToQueryParams query)
