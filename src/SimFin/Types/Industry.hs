{-|
Module      : SimFin.Types.Industry
Description : Parameterized sum type to distinguish between different industry specific data.
Copyright   : (c) Owen Shepherd, 2022
License     : MIT
Maintainer  : owen@owen.cafe
-}

module SimFin.Types.Industry
  ( Industry(..)
  , mapIndustry
  , invertIndustries
  ) where

import Data.Foldable

-- | Distinguish between different industry-specific data.

data Industry general bank insurance
  = General general
  | Bank bank
  | Insurance insurance
  deriving Show

-- | Map all discriminations of an Industry.

mapIndustry :: (a -> a') -> (b -> b') -> (c -> c') -> Industry a b c -> Industry a' b' c'
mapIndustry f g h industry = case industry of
  General a -> General $ f a
  Bank a -> Bank $ g a
  Insurance a -> Insurance $ h a

invertIndustry :: Industry [a] [b] [c] -> [Industry a b c]
invertIndustry ind = case ind of
  General as -> General <$> as
  Bank bs -> Bank <$> bs
  Insurance cs -> Insurance <$> cs

-- | List of discriminations of lists to list of discriminations.
-- | Used to removing extra nesting from the statements API results.

invertIndustries :: [Industry [a] [b] [c]] -> [Industry a b c]
invertIndustries = fold . fmap invertIndustry