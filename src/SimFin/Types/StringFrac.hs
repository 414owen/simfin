module SimFin.Types.StringFrac
  ( StringFrac(..)
  ) where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.Text as T
import Text.Read (readEither)

newtype StringFrac a = StringFrac { unStringFrac :: a }
  deriving Show

instance (Read a, RealFrac a) => FromJSON (StringFrac a) where
  parseJSON (String s) = case readEither $ T.unpack s of
    Left err -> fail err
    Right a -> pure $ StringFrac a
  parseJSON (Number n) = pure $ StringFrac $ realToFrac n
  parseJSON v = typeMismatch "Number or String" v

