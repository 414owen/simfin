{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module SimFin.Types.PricesQuery
  ( PricesQuery(..)
  , pricesQueryToQueryParams
  ) where

import Data.ByteString (ByteString)
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

pricesQueryToQueryParams :: PricesQuery -> [(ByteString, Maybe ByteString)]
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

