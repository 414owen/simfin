module Main where

{-
debugRequest
  :: ( MonadIO m
     , FromJSON a
     )
  => SimFinContext
  -> ByteString
  -> [QueryParam]
  -> m a
debugRequest SimFinContext{..} path query = do
  let req = makeRequest simFinApiKey path query
  res <- liftIO $ httpLbs req simFinManager
  case eitherDecode $ responseBody res of
    Left s -> do
      liftIO $ putStrLn s
      liftIO $ putStrLn $ show res
      undefined
    Right a -> pure a
-}


{-
testStatementQuery :: Text -> StatementQuery
testStatementQuery ref = StatementQuery
  { stockRefs = NE.singleton $ Ticker ref
  , periods = [FullYear]
  , years = [2020]
  , start = Nothing
  , end = Nothing
  , ttm = False
  , asReported = False
  , shares = False
  }

testPricesQuery :: Text -> PricesQuery
testPricesQuery ref = PricesQuery
  { stockRefs = NE.singleton $ Ticker ref
  , start = Nothing
  , end = Nothing
  , asReported = False
  }

test :: IO ()
test = do
  ctx <- createDefaultContext
  -- print =<< fetchBalanceSheets ctx (testStatementQuery"AAPL")
  -- print =<< fetchBalanceSheets ctx (testStatementQuery "CB")
  -- print =<< fetchBalanceSheets ctx (testStatementQuery "C")
  -- print =<< fetchProfitsAndLosses ctx (testStatementQuery "GOOG")
  -- print =<< fetchProfitsAndLosses ctx (testStatementQuery "C")
  -- print =<< fetchProfitsAndLosses ctx (testStatementQuery "CB")
  -- print =<< fetchCashFlows ctx (testStatementQuery "GOOG")
  -- print =<< fetchCashFlows ctx (testStatementQuery "C")
  -- print =<< fetchCashFlows ctx (testStatementQuery "CB")
  -- print =<< fetchDerived ctx (testStatementQuery "AAPL")
  -- print =<< fetchPrices ctx (testPricesQuery "AAPL")
  print =<< fetchPricesAndRatios ctx (testPricesQuery "AAPL")
  pure ()
-}

