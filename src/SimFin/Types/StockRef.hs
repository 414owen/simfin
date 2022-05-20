{-|
Module      : SimFin.Types.StockRef
Description : Type to represent a reference to a company.
Copyright   : (c) Owen Shepherd, 2022
License     : MIT
Maintainer  : owen@owen.cafe
-}

{-# LANGUAGE OverloadedStrings #-}

module SimFin.Types.StockRef
  ( StockRef(..)
  , stockRefsToQueryParams
  ) where

import Control.Arrow (first, second)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty)
import Data.String
import Data.Text (Text)
import qualified Data.Text as T

import SimFin.Internal

-- | A stock ref is a SimSin ID or a ticker.

data StockRef = SimFinId Int | Ticker Text
  deriving Show

instance IsString StockRef where
  fromString = Ticker . T.pack

-- | Collection of discriminations to discrimination of collections.

separateStockRefs :: Foldable t => t StockRef -> ([Int], [Text])
separateStockRefs = foldl' f ([], [])
  where
    f :: ([Int], [Text]) -> StockRef -> ([Int], [Text])
    f acc (SimFinId n) = first (n:) acc
    f acc (Ticker t) = second (t:) acc

-- | Convert one or more stock references into a list of query parameters.

stockRefsToQueryParams :: NonEmpty StockRef -> [QueryParam]
stockRefsToQueryParams refs =
  let
    (ids, tickers) = separateStockRefs refs
    tickerParam = toTextCommaQueryParam "ticker" tickers
    idParam = toShownCommaQueryParam "id" ids
  in tickerParam <> idParam

