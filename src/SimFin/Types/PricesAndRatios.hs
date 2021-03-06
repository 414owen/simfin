{-|
Module      : SimFin.Types.Prices
Description : Type to represent a combination of SimFin prices and ratios.
Copyright   : (c) Owen Shepherd, 2022
License     : MIT
Maintainer  : owen@owen.cafe
-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module SimFin.Types.PricesAndRatios
  ( PricesAndRatiosRow(..)
  , PricesAndRatiosKeyed(..)
  ) where

import Data.Aeson

import SimFin.Types.Prices
import SimFin.Types.Ratios
import SimFin.Internal

-- | Represents a company's prices and ratios.

data PricesAndRatiosRow a
  = PricesAndRatiosRow
  { prices :: PricesRow a
  , ratios :: RatiosRow a
  } deriving (Functor, Show)

instance (Read a, RealFrac a) => FromJSON (PricesAndRatiosRow a) where
  parseJSON v = PricesAndRatiosRow
    <$> parseJSON v
    <*> parseJSON v

-- | Wrapper to parse a PricesAndRatiosRow record from SimFin's JSON format.
-- You probably don't want to use this.

newtype PricesAndRatiosKeyed a = PricesAndRatiosKeyed { unKeyPricesAndRatios :: [PricesAndRatiosRow a] }

instance (Read a, RealFrac a) => FromJSON (PricesAndRatiosKeyed a) where
  parseJSON o = PricesAndRatiosKeyed <$> (traverse parseJSON =<< createKeyedRows o)
