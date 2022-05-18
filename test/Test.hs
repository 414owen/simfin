{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import SimFin.Free

testStatementQuery :: Text -> StatementQuery
testStatementQuery ref = StatementQuery
  { stockRefs = pure $ Ticker ref
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
  { stockRefs = pure $ Ticker ref
  , start = Nothing
  , end = Nothing
  , asReported = False
  }

type UnitIndustry = Industry () () ()

general :: UnitIndustry
general = General ()

bank :: UnitIndustry
bank = Bank ()

insurance :: UnitIndustry
insurance = Insurance ()

industryMatches :: Industry a b c -> Industry d e f -> Bool
industryMatches a b = case (a, b) of
  (General _, General _) -> True
  (Bank _, Bank _) -> True
  (Insurance _, Insurance _) -> True
  _ -> False

testFetchIndustry :: UnitIndustry -> IO (ApiResult (Maybe (Industry a b c))) -> IO ()
testFetchIndustry ind f = do
  res <- f
  case res of
    Right (Just a) -> assertBool "Industry matches" $ industryMatches ind a
    Left a -> assertFailure $ "Retrieved Left: " <> show a
    Right (Nothing) -> assertFailure "Retrieved Nothing successfully"

testFetch :: IO (ApiResult (Maybe a)) -> IO ()
testFetch f = do
  res <- f
  case res of
    Right (Just a) -> pure ()
    Left a -> assertFailure $ "Retrieved Left: " <> show a
    Right (Nothing) -> assertFailure "Retrieved Nothing successfully"

tests :: SimFinContext -> TestTree
tests ctx = testGroup "SimFin"
  [ testGroup "BalanceSheet"
    [ testCase "General" $ testBalanceSheet "AAPL" general
    , testCase "Bank" $ testBalanceSheet "C" bank
    , testCase "Insurance" $ testBalanceSheet "CB" insurance
    ]
  , testGroup "ProfitAndLoss"
    [ testCase "General" $ testProfitAndLoss "AAPL" general
    , testCase "Bank" $ testProfitAndLoss "C" bank
    , testCase "Insurance" $ testProfitAndLoss "CB" insurance
    ]
  , testGroup "CashFlow"
    [ testCase "General" $ testCashFlow "AAPL" general
    , testCase "Bank" $ testCashFlow "C" bank
    , testCase "Insurance" $ testCashFlow "CB" insurance
    ]
  , testGroup "Derived"
    [ testCase "General" $ testDerived "AAPL"
    , testCase "Bank" $ testDerived "C"
    , testCase "Insurance" $ testDerived "CB"
    ]
  ]
  where
    testStmt :: (SimFinContext -> StatementQuery -> IO a) -> Text -> IO a
    testStmt f ticker = f ctx $ testStatementQuery ticker

    testBalanceSheet :: Text -> UnitIndustry -> Assertion
    testBalanceSheet ticker industry = testFetchIndustry industry $ testStmt fetchBalanceSheet ticker

    testProfitAndLoss :: Text -> UnitIndustry -> Assertion
    testProfitAndLoss ticker industry = testFetchIndustry industry $ testStmt fetchProfitAndLoss ticker

    testCashFlow :: Text -> UnitIndustry -> Assertion
    testCashFlow ticker industry = testFetchIndustry industry $ testStmt fetchCashFlow ticker

    testDerived :: Text -> Assertion
    testDerived ticker = testFetch $ testStmt fetchDerived ticker
    -- print =<< fetchDerived ctx (testStatementQuery "AAPL")
    -- print =<< fetchPrices ctx (testPricesQuery "AAPL")
    -- print =<< fetchPricesAndRatios ctx (testPricesQuery "AAPL")

main :: IO ()
main = do
  ctx <- createDefaultContext
  defaultMain (tests ctx)
