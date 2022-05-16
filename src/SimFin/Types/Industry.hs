module SimFin.Types.Industry
  ( Industry(..)
  , mapIndustry
  ) where

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

