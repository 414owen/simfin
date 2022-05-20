{-|
Module      : SimFin.Types.StringFrac
Description : Wrapper for parsing JSON strings or numbers as a RealFrac.
Copyright   : (c) Owen Shepherd, 2022
License     : MIT
Maintainer  : owen@owen.cafe
-}

module SimFin.Types.StringFrac
  ( StringFrac(..)
  ) where

import Data.Aeson hiding (String)
import qualified Data.Aeson as A
import Data.Aeson.Types (typeMismatch)
import qualified Data.Text as T
import Text.Read (readEither)

-- | Wrapper that parses the string '"1.23"' and the number '1.23' the same.
-- | Uses the read instance for the 'String', and 'realToFrac' (from 'Data.Scientific.Scientific')
-- | for the number.

newtype StringFrac a = StringFrac { unStringFrac :: a }
  deriving Show

instance (Read a, RealFrac a) => FromJSON (StringFrac a) where
  parseJSON (A.String s) = case readEither $ T.unpack s of
    Left err -> fail err
    Right a -> pure $ StringFrac a
  parseJSON (Number n) = pure $ StringFrac $ realToFrac n
  parseJSON v = typeMismatch "Number or String" v
