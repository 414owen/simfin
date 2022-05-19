{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import SimFin.Free

pricesQuery :: Ticker -> PricesQueryFree
pricesQuery ticker = PricesQueryFree
  { stockRef = ticker
  , start = Just 2020
  , end = Just 2021
  }

tickers

main :: IO ()
main = do
  ctx <- createDefaultContext

  googPrices <- fetchPrices ctx $ pricesQuery "GOOG"

  toFile def "prices.png" $ do
    layout_title .= "Price History"
    layout_background .= solidFillStyle (opaque white)
    layout_foreground .= (opaque black)
    layout_left_axis_visibility . axis_show_ticks .= False

    plot (line "GOOG" [ [ (date, close) | Prices{..} <- prices ] ] )

    plot $ liftEC $ do
      area_spots_4d_max_radius .= 20
      area_spots_4d_values .= values
  pure ()
