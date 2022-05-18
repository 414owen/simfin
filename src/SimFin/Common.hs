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
import SimFin.Util


data ApiError
  -- Can't turn ByteString into JSON
  = DecodeError LBS.ByteString String
  -- Can't turn JSON into result type
  | ParseError Value String
  -- This could in theory be parsed into machine-readable format,
  -- with variants such as `InvalidApiKey | RateLimited | ...`, but 
  -- the API doesn't guarantee error message stability.
  | Other Text

instance Show ApiError where
  show (DecodeError _ err) = "Couldn't decode reponse body. Err: " <> err
  show (ParseError _ err) = "Couldn't parse JSON value. Err: " <> err
  show (Other err) = "Server returned error: " <> T.unpack err

type ApiResult = Either ApiError

instance FromJSON ApiError where
  parseJSON = withObject "Root" $ \v -> Other <$> v .: "error"

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

fetchCompanyList
  :: (MonadThrow m, MonadIO m)
  => SimFinContext
  -> m (Either ApiError [CompanyListingRow])
fetchCompanyList ctx =
  fmap unKeyCompanyListing <$> performRequest ctx "companies/list" []
