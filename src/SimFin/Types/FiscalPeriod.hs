{-# LANGUAGE OverloadedStrings #-}

module SimFin.Types.FiscalPeriod
  ( FiscalPeriod(..)
  , fiscalPeriodParam
  ) where

import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.Text as T

data FiscalPeriod = Q1 | Q2 | Q3 | Q4 | H1 | H2 | FullYear | FirstNineMonths | SixMonths
  deriving (Eq, Show)

instance FromJSON FiscalPeriod where
  parseJSON = withText "FiscalPeriod" $ \t -> case T.toLower t of
    "q1" -> pure Q1
    "q2" -> pure Q2
    "q3" -> pure Q3
    "q4" -> pure Q4
    "h1" -> pure H1
    "h2" -> pure H2
    "fy" -> pure FullYear
    "9m" -> pure FirstNineMonths
    "6m" -> pure SixMonths
    _ -> fail "Invalid fiscal year string"

fiscalPeriodParam :: FiscalPeriod -> ByteString
fiscalPeriodParam a = case a of
  Q1 -> "q1"
  Q2 -> "q2"
  Q3 -> "q3"
  Q4 -> "q4"
  H1 -> "h1"
  H2 -> "h2"
  FullYear-> "fy"
  FirstNineMonths -> "9m"
  SixMonths -> "6m"
