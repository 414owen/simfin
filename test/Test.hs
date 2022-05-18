{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase#-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import SimFin.Free

main :: IO ()
main = do
  ctx <- createDefaultContext
  defaultMain (tests ctx)

testStatementQuery :: Text -> StatementQueryFree
testStatementQuery ref = StatementQueryFree
  { stockRef = Ticker ref
  , period = FullYear
  , year = 2020
  , ttm = False
  }

testPricesQuery :: Text -> PricesQueryFree
testPricesQuery ref = PricesQueryFree
  { stockRef = Ticker ref
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

testFetchMaybe :: IO (ApiResult (Maybe a)) -> IO ()
testFetchMaybe f = do
  res <- f
  flip withRight res $ withMaybe $ const $ pure ()

testFetchIndustry :: UnitIndustry -> IO (ApiResult (Maybe (Industry a b c))) -> IO ()
testFetchIndustry ind f = do
  res <- f
  flip withRight res $ withMaybe $ assertBool "Industry Matches" . industryMatches ind

failWithEmpty :: IO ()
failWithEmpty = assertFailure "Retrieved Nothing successfully"

testFetchList :: IO (ApiResult [a]) -> IO ()
testFetchList f = do
  res <- f
  withRight ensureNE res

withRight :: Show c => (a -> IO b) -> Either c a -> IO b
withRight _ (Left a) = assertFailure $ "Retrieved Left: " <> show a
withRight f (Right a) = f a

withMaybe :: (a -> IO ()) -> Maybe a -> IO ()
withMaybe _ Nothing = failWithEmpty
withMaybe f (Just a) = f a

ensureNE :: [a] -> IO ()
ensureNE [] = failWithEmpty
ensureNE _ = pure ()

tests :: SimFinContext -> TestTree
tests ctx = testGroup "SimFin"
  [ testCase "List companies" $ testFetchList $ fetchCompanyList ctx
  , testCase "Company Info" $ testFetchMaybe $ fetchCompanyInfo ctx "AAPL"
  , testGroup "Balance Sheet"
    [ testCase "General" $ testBalanceSheet "AAPL" general
    , testCase "Bank" $ testBalanceSheet "C" bank
    , testCase "Insurance" $ testBalanceSheet "CB" insurance
    ]
  , testGroup "Profit & Loss"
    [ testCase "General" $ testProfitAndLoss "AAPL" general
    , testCase "Bank" $ testProfitAndLoss "C" bank
    , testCase "Insurance" $ testProfitAndLoss "CB" insurance
    ]
  , testGroup "Cash Flow"
    [ testCase "General" $ testCashFlow "AAPL" general
    , testCase "Bank" $ testCashFlow "C" bank
    , testCase "Insurance" $ testCashFlow "CB" insurance
    ]
    -- This endpoint isn't different by industry types
  , testGroup "Derived"
    [ testCase "General" $ testDerived "AAPL"
    , testCase "Bank" $ testDerived "C"
    , testCase "Insurance" $ testDerived "CB"
    ]
  , testGroup "Price"
    [ testCase "General" $ testFetchPrices "AAPL"
    , testCase "Bank" $ testFetchPrices "C"
    , testCase "Insurance" $ testFetchPrices "CB"
    ]
  ]
  where
    testStmt :: (SimFinContext -> StatementQueryFree -> IO a) -> Text -> IO a
    testStmt f ticker = f ctx $ testStatementQuery ticker

    testBalanceSheet :: Text -> UnitIndustry -> Assertion
    testBalanceSheet ticker industry = testFetchIndustry industry $ testStmt fetchBalanceSheet ticker

    testProfitAndLoss :: Text -> UnitIndustry -> Assertion
    testProfitAndLoss ticker industry = testFetchIndustry industry $ testStmt fetchProfitAndLoss ticker

    testCashFlow :: Text -> UnitIndustry -> Assertion
    testCashFlow ticker industry = testFetchIndustry industry $ testStmt fetchCashFlow ticker

    testDerived :: Text -> Assertion
    testDerived ticker = testFetchMaybe $ testStmt fetchDerived ticker

    testFetchPrices :: Text -> IO ()
    testFetchPrices ticker = testFetchList $ fetchPrices ctx $ testPricesQuery ticker
