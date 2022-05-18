{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module SimFin.Types.PricesQuery
  ( PricesQuery(..)
  , PricesQueryFree(..)
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

data PricesQueryFree
  = PricesQueryFree
  { stockRef :: StockRef
  , start :: Maybe Day
  , end :: Maybe Day
  , asReported :: Bool
  } deriving Show

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
freeToPlus PricesQueryFree{..}
  = PricesQuery
  { stockRefs = pure stockRef
  , start = start
  , end = end
  , asReported = asReported
  }

pricesQueryFreeToQueryParams :: PricesQueryFree -> [QueryParam]
pricesQueryFreeToQueryParams = pricesQueryToQueryParams . freeToPlus
