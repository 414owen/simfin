{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
#else
import qualified Data.HashMap.Strict as HM
#endif
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Functor.Syntax
import Data.Text (Text)
import qualified Data.Text.Encoding as T

type QueryParam = (ByteString, Maybe ByteString)

#if MIN_VERSION_aeson(2,0,0)

toKey :: Text -> Key
toKey = K.fromText

toObject :: [(K.Key, Value)] -> Value
toObject = Object . KM.fromList

#else

toKey :: Text -> Text
toKey = id

toObject :: [(Text, Value)] -> Value
toObject = Object . HM.fromList 

#endif

createKeyedRow :: Value -> Parser Value
createKeyedRow = withObject "Root" $ \root -> do
  cols <- toKey <$$> root .: "columns"
  row <- root .: "data"
  pure $ toObject $ zip cols row

createKeyedRows :: Value -> Parser [Value]
createKeyedRows = withObject "Root" $ \root -> do
  cols <- toKey <$$> root .: "columns"
  rows <- root .: "data"
  pure $ toObject . zip cols <$> rows

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
