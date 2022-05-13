module SimFin where

import Network.HTTP.Client
import Data.Text

type ApiKey = Text
type SimFinId = Int
type ticker = Text

type CompanyListingRow = (SimFinId, Ticker)
type CompanyListing = [CompanyListingRow]

data SimFinContext = SimFinContext
  { apiKey :: ApiKey
  , unContext :: Manager
  }

mkManager :: ApiKey -> ManagerSettings -> IO Manager
mkManager

apiKeyEnvVariable :: String
apiKeyEnvVariable = "SIM_FIN_API_KEY"

createDefaultManager :: (MonadFail m, MonadIO m) => m SimFinContext
createDefaultManager = do
  manager <- newTlsManagerWith
  apiKeyOpt <- getEnv apiKeyEnvVariable
  case apiKeyOpt of
    Nothing -> fail $ "Couldn't find environment variable '" <> apiKeyEnvVariable <> "'"
    Just apiKey -> pure $ SimFinContext apiKey manager

listCompanies :: ApiKey -> CompanyListing
listCompanies key = 