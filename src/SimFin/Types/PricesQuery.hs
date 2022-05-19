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
import SimFin.Util

data PricesQuery
  = PricesQuery
  { stockRefs :: NonEmpty StockRef
  , start :: Maybe Day
  , end :: Maybe Day
  , asReported :: Bool
  } deriving Show

type PricesQueryFree = StockRef

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

pricesQueryFreeToQueryParams :: PricesQueryFree -> [QueryParam]
pricesQueryFreeToQueryParams = pricesQueryToQueryParams . freeToPlus
