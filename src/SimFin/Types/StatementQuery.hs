{-|
Module      : SimFin.Types.StatementQuery
Description : Types to represent SimFin statement queries.
Copyright   : (c) Owen Shepherd, 2022
License     : MIT
Maintainer  : owen@owen.cafe
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module SimFin.Types.StatementQuery
  ( StatementQuery(..)
  , StatementQueryFree(..)
  , statementQueryToQueryParams
  , statementQueryFreeToQueryParams
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (maybeToList)
import Data.Time.Calendar (Day)

import SimFin.Types.FiscalPeriod
import SimFin.Types.StockRef
import SimFin.Internal

-- | This represents all options the statement endpoint supports, minus the "statement"
-- | parameter itself, which is set by simply calling the right function.
-- | Some of these parameters are only available to SimFin+ users.
-- | For free users, please use 'StatementQueryFree'.
-- | If you provide a zero-length list for any field, the query parameter will be omitted,
-- | and the API will try to return all relevant available statements.

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

-- | Turn a 'StatementQuery' into query parameters for the SimFin "statements" endpoint.

statementQueryToQueryParams :: StatementQuery -> [QueryParam]
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

-- | This is a subset of the StatementQuery type, which models the parameters available
-- | to non-SimFin+ users.

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

-- | Turn a 'StatementQueryFree' into query parameters for the SimFin "statements" endpoint.

statementQueryFreeToQueryParams :: StatementQueryFree -> [QueryParam]
statementQueryFreeToQueryParams = statementQueryToQueryParams . freeStatementQueryToPaidStatementQuery
