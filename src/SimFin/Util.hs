{-# LANGUAGE OverloadedStrings #-}
module SimFin.Util
  ( QueryParam
  , createKeyedRow
  , createKeyedRows
  , toCommaQueryParam
  , toBoolQueryParam
  , toTextCommaQueryParam
  , toShownCommaQueryParam
  ) where

import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text.Encoding as T

type QueryParam = (ByteString, Maybe ByteString)

createKeyedRow :: Value -> Parser Value
createKeyedRow = withObject "Root" $ \root -> do
  cols :: [Key] <- fmap K.fromText <$> root .: "columns"
  row :: [Value] <- root .: "data"
  pure $ Object $ KM.fromList $ zip cols row

createKeyedRows :: Value -> Parser [Value]
createKeyedRows = withObject "Root" $ \root -> do
  cols :: [Key] <- fmap K.fromText <$> root .: "columns"
  rows :: [[Value]] <- root .: "data"
  pure $ Object . KM.fromList . zip cols <$> rows

toCommaQueryParam :: ByteString -> (a -> ByteString) -> [a] -> [QueryParam]
toCommaQueryParam key f as = case as of
  [] -> []
  _ -> [(key, Just $ BS8.intercalate "," $ f <$> as)]

-- Chars are truncated to 8-bits
toShownCommaQueryParam :: Show a => ByteString -> [a] -> [QueryParam]
toShownCommaQueryParam key = toCommaQueryParam key (BS8.pack . show)

toTextCommaQueryParam :: ByteString -> [Text] -> [QueryParam]
toTextCommaQueryParam key = toCommaQueryParam key T.encodeUtf8

toBoolQueryParam :: ByteString -> Bool -> [QueryParam]
toBoolQueryParam key b = case b of
  False -> []
  True -> [(key, Nothing)]
