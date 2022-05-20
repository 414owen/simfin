{-|
Module      : SimFin.Types.Common
Description : Types and functions common to the free and paid APIs.
Copyright   : (c) Owen Shepherd, 2022
License     : MIT
Maintainer  : owen@owen.cafe
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module SimFin.Common
  ( ApiError(..)
  , ApiResult
  , fetchCompanyList
  , performRequest
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T

import SimFin.Internal
import SimFin.Types.CompanyListing

-- | Represents all the types of error the server returns, or we can encounter
-- | on our side.

data ApiError
  -- | Can't turn ByteString into JSON
  = DecodeError LBS.ByteString String
  -- | Can't turn JSON into result type
  | ParseError Value String
  -- | Server returned '{"error": "..."}' along with a non-200 status code.
  -- This could in theory be parsed into machine-readable format,
  -- with variants such as `InvalidApiKey | RateLimited | ...`, but 
  -- the API doesn't guarantee error message stability.
  | Other Text

instance Show ApiError where
  show (DecodeError _ err) = "Couldn't decode reponse body. Err: " <> err
  show (ParseError _ err) = "Couldn't parse JSON value. Err: " <> err
  show (Other err) = "Server returned error: " <> T.unpack err

-- | The result of calling fetch* is either an error or a successful result.

type ApiResult = Either ApiError

instance FromJSON ApiError where
  parseJSON = withObject "Root" $ \v -> Other <$> v .: "error"

-- | Make a request, all fetch* functions call this.

performRequest
  :: ( MonadIO m
     , FromJSON a
     )
  => SimFinContext
  -> ByteString
  -> [QueryParam]
  -> m (ApiResult a)
performRequest = performGenericRequest DecodeError ParseError


------
-- List companies
------

-- | Fetch a list of company tickers and SimFin ids.
-- | This is the only endpoint common to free and paid customers.

fetchCompanyList
  :: (MonadThrow m, MonadIO m)
  => SimFinContext
  -> m (Either ApiError [CompanyListingRow])
fetchCompanyList ctx =
  fmap unKeyCompanyListing <$> performRequest ctx "companies/list" []
