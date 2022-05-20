{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main
  ( main
  ) where

import Control.Arrow (first, second)
import Control.Monad
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Either
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.Rendering.Chart.Easy hiding (close)
import Graphics.Rendering.Chart.Backend.Diagrams

import SimFin.Free

type LinePt = (LocalTime, Double)

toPoint :: PricesRow Double -> Maybe LinePt
toPoint row = do
  day <- date row
  pure (to5pm day, close row)

stonks :: [StockRef]
stonks = ["GOOG", "AAPL", "TWTR", "NFLX"]

type LineData = (String, NonEmpty LinePt)

to5pm :: Day -> LocalTime
to5pm = flip LocalTime $ dayFractionToTimeOfDay $ 17 / 24

ptsAfter :: LocalTime -> NonEmpty LinePt -> Maybe (NonEmpty LinePt)
ptsAfter t = NE.nonEmpty . filter ((>= t) . fst) . NE.toList

toLine :: [PricesRow Double] -> Maybe (Text, NonEmpty LinePt)
toLine [] = Nothing
toLine pts@(PricesRow{ticker = t} : _) = (t,) <$> NE.nonEmpty (catMaybes $ toPoint <$> pts)

getStart :: LineData -> LocalTime
getStart (_, (s, _) :| _) = s

lastStart :: [LineData] -> LocalTime
lastStart = maximum . fmap getStart

afterCommonStart :: [LineData] -> [LineData]
afterCommonStart ls = catMaybes $ traverse (ptsAfter $ lastStart ls) <$> ls

scaleToRelative :: NonEmpty LinePt -> NonEmpty LinePt
scaleToRelative xs@((_, x) :| _) = second ((/ x) . (* 100)) <$> xs

toLines :: [[PricesRow Double]] -> [LineData]
toLines pts = fmap (second scaleToRelative)
  $ afterCommonStart
  $ first T.unpack
  <$> catMaybes (toLine <$> pts)

main :: IO ()
main = do
  ctx <- createDefaultContext

  pricesRes :: [ApiResult [PricesRow Double]] <- traverse (fetchPrices ctx) stonks
  let prices :: [LineData] = toLines $ rights pricesRes

  toFile def "relative-prices.svg" $ do
    layout_title .= "Price History"
    layout_background .= solidFillStyle (opaque white)
    layout_foreground .= opaque black
    layout_left_axis_visibility . axis_show_ticks .= False
    layout_x_axis . laxis_title .= "Date"
    layout_y_axis . laxis_title .= "Performance (%)"
    forM_ prices $ \(name, pts) -> plot $ line name [NE.toList pts]
  pure ()
