{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main
  ) where

import Control.Arrow (first, second, (&&&))
import Control.Monad
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Multimap as MM
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.Rendering.Chart.Easy hiding (close)
import Graphics.Rendering.Chart.Backend.Diagrams

import SimFin.Free
import qualified SimFin.Types.Derived as D

type LinePt = (LocalTime, Double)
type LineData = (String, NonEmpty LinePt)

toPoint :: DerivedRow Double -> Maybe LinePt
toPoint row = do
  eps <- earningsPerShareDiluted row
  let day = reportDate (row :: DerivedRow Double)
  pure (to5pm day, eps)

stocks :: [StockRef]
stocks = ["GOOG", "AAPL", "TWTR", "NFLX"]

-- Caution! This can blow through API limits.
stockQueries :: [StatementQueryFree]
stockQueries = do
  stock <- stocks
  year <- [2016..2020]
  period <- [Q1, Q2, Q3, Q4]
  pure $ StatementQueryFree
    { stockRef = stock
    , period = period
    , year = year
    , ttm = False
    }

to5pm :: Day -> LocalTime
to5pm = flip LocalTime $ dayFractionToTimeOfDay $ 17 / 24

day2015 :: LocalTime
day2015 = LocalTime (fromGregorian 2015 01 01) midnight

ptsAfter :: LocalTime -> NonEmpty LinePt -> Maybe (NonEmpty LinePt)
ptsAfter t = NE.nonEmpty . filter ((>= t) . fst) . NE.toList

getStart :: LineData -> LocalTime
getStart (_, (s, _) :| _) = s

lastStart :: [LineData] -> LocalTime
lastStart = maximum . fmap getStart

afterCommonStart :: [LineData] -> [LineData]
afterCommonStart ls = catMaybes $ traverse (ptsAfter $ lastStart ls) <$> ls

scaleToRelative :: NonEmpty LinePt -> NonEmpty LinePt
scaleToRelative xs@((_, x) :| _) = (second ((+ (-100)) . (/ x) . (* 100))) <$> xs

groupOn' :: Ord b => (a -> b) -> [a] -> [(b, NonEmpty a)]
groupOn' f lst = M.toList $ MM.toMap $ MM.fromList $ (f &&& id) <$> lst

toLines :: [DerivedRow Double] -> [LineData]
toLines rows =
  let
    groups :: [LineData]
    groups = do
      (ticker, rows') <- groupOn' D.ticker rows
      pts <- maybeToList $ traverse toPoint rows'
      pure (T.unpack ticker, pts)
  in 
  fmap (second scaleToRelative)
    $ afterCommonStart
    $ groups

main :: IO ()
main = do
  ctx <- createDefaultContext

  pricesRes :: [ApiResult (Maybe (DerivedRow Double))] <- traverse (fetchDerived ctx) stockQueries
  let prices :: [LineData] = toLines $ catMaybes $ rights pricesRes

  toFile def "relative-eps-changes.svg" $ do
    layout_title .= "Price History"
    layout_background .= solidFillStyle (opaque white)
    layout_foreground .= opaque black
    layout_left_axis_visibility . axis_show_ticks .= False
    layout_x_axis . laxis_title .= "Date"
    layout_y_axis . laxis_title .= "EPS change since Q1 2016 (%)"
    forM_ prices $ \(name, pts) -> plot $ line name [NE.toList pts]
  pure ()
