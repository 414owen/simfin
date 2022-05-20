{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main
  ) where

import Control.Arrow ((&&&))
import Control.Monad
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Multimap as MM
import Data.Either
import qualified Data.Text as T
import Graphics.Rendering.Chart.Easy hiding (close)
import Graphics.Rendering.Chart.Backend.Diagrams

import SimFin.Free
import qualified SimFin.Types.Derived as D

type LinePt = (LocalTime, Double)
type LineData = (String, NonEmpty LinePt)

to5pm :: Day -> LocalTime
to5pm = flip LocalTime $ dayFractionToTimeOfDay $ 17 / 24

toPoint :: DerivedRow Double -> Maybe LinePt
toPoint row = do
  eps <- earningsPerShareDiluted row
  let day = reportDate (row :: DerivedRow Double)
  pure (to5pm day, eps)

stocks :: [StockRef]
stocks = ["GOOG", "AAPL", "TWTR", "NFLX"]

-- Caution! This can blow through API limits.
stockQueries :: Integer -> [StatementQueryFree]
stockQueries year = do
  stock <- stocks
  year <- [2016..fromInteger year]
  period <- [Q1, Q2, Q3, Q4]
  pure $ StatementQueryFree
    { stockRef = stock
    , period = period
    , year = year
    , ttm = False
    }

groupOn' :: Ord b => (a -> b) -> [a] -> [(b, NonEmpty a)]
groupOn' f lst = M.toList $ MM.toMap $ MM.fromList $ (f &&& id) <$> lst

toLines :: [DerivedRow Double] -> [LineData]
toLines rows = do
  (ticker, rows') <- groupOn' D.ticker rows
  pts <- maybeToList $ traverse toPoint rows'
  pure (T.unpack ticker, pts)

main :: IO ()
main = do
  now <- getCurrentTime
  let (year, _, _) = toGregorian $ utctDay now
  ctx <- createDefaultContext

  pricesRes :: [ApiResult (Maybe (DerivedRow Double))] <- traverse (fetchDerived ctx) $ stockQueries year
  let prices :: [LineData] = toLines $ catMaybes $ rights pricesRes

  toFile def "eps-diluted.svg" $ do
    layout_title .= "Price History"
    layout_background .= solidFillStyle (opaque white)
    layout_foreground .= opaque black
    layout_left_axis_visibility . axis_show_ticks .= False
    layout_x_axis . laxis_title .= "Date"
    layout_y_axis . laxis_title .= "Diluted Earnings per Share ($)"
    forM_ prices $ \(name, pts) -> plot $ line name [NE.toList pts]
  pure ()
