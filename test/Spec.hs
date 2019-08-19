{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Database

import Data.Time.Clock(getCurrentTime)

main :: IO ()
main = hspec $ do

  describe "Database" $ do
    it "inserts FeedItem" $ do

      now <- getCurrentTime
      let item = FeedItem "123" "Llamas for Fun and Profit" "http://llama.org/llamas-for-fun-and-profit" now
      conn <- connect connectInfo
      migrateWith conn
      r <- insertFeedItem conn item
      print r
      return ()
