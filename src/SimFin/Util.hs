{-# LANGUAGE OverloadedStrings #-}
module SimFin.Util
  ( createKeyedRow
  , createKeyedRows
  ) where

import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)

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
