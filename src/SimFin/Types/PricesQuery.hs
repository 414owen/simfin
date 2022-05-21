{-|
Module      : SimFin.Types.PricesQuery
Description : Types to represent SimFin price queries.
Copyright   : (c) Owen Shepherd, 2022
License     : MIT
Maintainer  : owen@owen.cafe
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module SimFin.Types.PricesQuery
  ( PricesQuery(..)
  , PricesQueryFree
  , pricesQueryToQueryParams
  , pricesQueryFreeToQueryParams
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (maybeToList)
import Data.Time.Calendar (Day)

import SimFin.Types.StockRef
import SimFin.Internal

-- | This represents all options the prices endpoint supports.
-- | Some of these parameters are only available to SimFin+ users.
-- | For free users, please use 'PricesQueryFree'.

data PricesQuery
  = PricesQuery
  { stockRefs :: NonEmpty StockRef
  , start :: Maybe Day
  , end :: Maybe Day
  , asReported :: Bool
  } deriving Show

-- | Represents all the parameters available to free users.

type PricesQueryFree = StockRef

-- | Turn a 'PricesQuery' into query parameters for the SimFin "prices" endpoint.

pricesQueryToQueryParams :: PricesQuery -> [QueryParam]
pricesQueryToQueryParams PricesQuery{..} = 
  let
    refParams = stockRefsToQueryParams stockRefs
    startParam = toShownCommaQueryParam "start "$ maybeToList start
    endParam = toShownCommaQueryParam "end" $ maybeToList end
    asReportedParam = toBoolQueryParam "asreported" asReported
  in
  mconcat
    [ refParams
    , startParam
    , endParam
    , asReportedParam
    ]

freeToPlus :: PricesQueryFree -> PricesQuery
freeToPlus stockRef
  = PricesQuery
  { stockRefs = pure stockRef
  , start = Nothing
  , end = Nothing
  , asReported = False
  }

-- | Turn a 'PricesQueryFree' into query parameters for the SimFin "prices" endpoint.

pricesQueryFreeToQueryParams :: PricesQueryFree -> [QueryParam]
pricesQueryFreeToQueryParams = pricesQueryToQueryParams . freeToPlus
