{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SimFin.Internal
  ( SimFinContext(..)
  , QueryParam
  , createKeyedRow
  , createKeyedRows
  , toCommaQueryParam
  , toBoolQueryParam
  , toTextCommaQueryParam
  , toShownCommaQueryParam
  , baseRequest
  , makeRequest
  , performGenericRequest
  ) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types (parse, Parser)

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
#else
import qualified Data.HashMap.Strict as HM
#endif

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Functor.Syntax
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Network.HTTP.Client
import Network.HTTP.Types.Status

-- | The context needed to call every fetch* function.

data SimFinContext = SimFinContext
  { simFinApiKey :: ByteString
  , simFinManager :: Manager
  }

baseRequest :: Request
baseRequest = defaultRequest
  { host = "simfin.com"
  , port = 443
  , secure = True
  , requestHeaders =
    [ ("Accept", "application/json")
    ]
  }

basePath :: ByteString
basePath = "/api/v2/"

makeRequest :: ByteString -> ByteString -> [QueryParam] -> Request
makeRequest apiKey path query =
  setQueryString (("api-key", Just apiKey) : query)
  $ baseRequest { path = basePath <> path }

performGenericRequest
  :: ( MonadIO m
     , FromJSON a
     , FromJSON e
     )
  => (LBS.ByteString -> String -> e)
  -> (Value -> String -> e)
  -> SimFinContext
  -> ByteString
  -> [QueryParam]
  -> m (Either e a)
performGenericRequest mkDecodeErr mkParseErr SimFinContext{..} path query = do
  let req = makeRequest simFinApiKey path query
  res <- liftIO $ httpLbs req simFinManager
  let body = responseBody res
  -- Try to parse body into generic JSON
  pure $ case eitherDecode $ responseBody res of
    Left err -> Left $ mkDecodeErr body err
    Right value -> case statusCode $ responseStatus res of
      200 -> case parse parseJSON value of
        Error err -> Left $ mkParseErr value err
        Success a -> Right a
      _ -> case parse parseJSON value of
        Error err -> Left $ mkParseErr value err
        Success a -> Left a

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
