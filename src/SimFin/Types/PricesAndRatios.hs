{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module SimFin.Types.PricesAndRatios
  ( PricesAndRatiosRow(..)
  , PricesAndRatiosKeyed(..)
  ) where

import Data.Aeson

import SimFin.Types.Prices
import SimFin.Types.Ratios
import SimFin.Util

data PricesAndRatiosRow a
  = PricesAndRatiosRow
  { prices :: PricesRow a
  , ratios :: RatiosRow a
  } deriving Show

instance (Read a, RealFrac a) => FromJSON (PricesAndRatiosRow a) where
  parseJSON v = PricesAndRatiosRow
    <$> parseJSON v
    <*> parseJSON v

newtype PricesAndRatiosKeyed a = PricesAndRatiosKeyed { unKeyPricesAndRatios :: [PricesAndRatiosRow a] }

instance (Read a, RealFrac a) => FromJSON (PricesAndRatiosKeyed a) where
  parseJSON o = PricesAndRatiosKeyed <$> (traverse parseJSON =<< createKeyedRows o)
