{-# LANGUAGE OverloadedStrings #-}

module SimFin.Types.StockRef
  ( StockRef(..)
  , separateStockRefs
  , stockRefsToQueryParams
  ) where

import Control.Arrow (first, second)
import Data.ByteString (ByteString)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty)
import Data.String
import Data.Text (Text)
import qualified Data.Text as T

import SimFin.Util

data StockRef = SimFinId Int | Ticker Text
  deriving Show

instance IsString StockRef where
  fromString = Ticker . T.pack

separateStockRefs :: Foldable t => t StockRef -> ([Int], [Text])
separateStockRefs = foldl' f ([], [])
  where
    f :: ([Int], [Text]) -> StockRef -> ([Int], [Text])
    f acc (SimFinId n) = first (n:) acc
    f acc (Ticker t) = second (t:) acc

stockRefsToQueryParams :: NonEmpty StockRef -> [(ByteString, Maybe ByteString)]
stockRefsToQueryParams refs =
  let
    (ids, tickers) = separateStockRefs refs
    tickerParam = toTextCommaQueryParam "ticker" tickers
    idParam = toShownCommaQueryParam "id" ids
  in tickerParam <> idParam

