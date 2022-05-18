module SimFin.Types.Industry
  ( Industry(..)
  , mapIndustry
  , invertIndustries
  ) where

import Data.Foldable

data Industry general bank insurance
  = General general
  | Bank bank
  | Insurance insurance
  deriving Show

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

invertIndustries :: [Industry [a] [b] [c]] -> [Industry a b c]
invertIndustries = fold . fmap invertIndustry