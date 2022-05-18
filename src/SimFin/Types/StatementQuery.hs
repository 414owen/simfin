{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module SimFin.Types.StatementQuery
  ( StatementQuery(..)
  , statementQueryToQueryParams
  , statementQueryFreeToQueryParams
  ) where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (maybeToList)
import Data.Time.Calendar (Day)

import SimFin.Types.FiscalPeriod
import SimFin.Types.StockRef
import SimFin.Util

-- For SimFin+ users
data StatementQuery
  = StatementQuery
  { stockRefs :: NonEmpty StockRef
  , periods :: [FiscalPeriod]
  , years :: [Int]
  , start :: Maybe Day
  , end :: Maybe Day
  , ttm :: Bool
  , asReported :: Bool
  -- TODO we don't model the result of this yet
  , shares :: Bool
  } deriving Show

statementQueryToQueryParams :: StatementQuery -> [(ByteString, Maybe ByteString)]
statementQueryToQueryParams StatementQuery{..} =
  let
    refParams = stockRefsToQueryParams stockRefs
    startParam = toShownCommaQueryParam "start "$ maybeToList start
    endParam = toShownCommaQueryParam "end" $ maybeToList end
    periodParam = toCommaQueryParam "period" fiscalPeriodParam periods
    yearParam = toShownCommaQueryParam "fyear" years
    ttmParam = toBoolQueryParam "ttm" ttm
    asReportedParam = toBoolQueryParam "asreported" asReported
    sharesParam = toBoolQueryParam "shares" shares
  in
  mconcat
    [ refParams
    , startParam
    , endParam
    , periodParam
    , yearParam
    , ttmParam
    , asReportedParam
    , sharesParam
    ]

-- For SimFin- users
-- This is a subset of the SimFin+ API
data StatementQueryFree
  = StatementQueryFree
  { stockRef :: StockRef
  , period :: FiscalPeriod
  , year :: Int
  , ttm :: Bool
  }

freeStatementQueryToPaidStatementQuery :: StatementQueryFree -> StatementQuery
freeStatementQueryToPaidStatementQuery StatementQueryFree{..}
  = StatementQuery
  { stockRefs = pure stockRef
  , periods = pure period
  , years = pure year
  , start = Nothing
  , end = Nothing
  , ttm = ttm
  , asReported = False
  , shares = False
  }

statementQueryFreeToQueryParams :: StatementQueryFree -> [(ByteString, Maybe ByteString)]
statementQueryFreeToQueryParams = statementQueryToQueryParams . freeStatementQueryToPaidStatementQuery
