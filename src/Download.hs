{-# LANGUAGE OverloadedStrings #-}

module Download (downloadFeed, importItems, importFeeds) where

import Control.Monad.IO.Class

import Network.HTTP.Req
import Text.Feed.Types
import Text.Feed.Import
import Text.Feed.Query
import Data.ByteString.Lazy(fromStrict)
import Database

import Control.Monad (void)

import Data.Maybe (catMaybes)
import Data.Time (parseTimeOrError, defaultTimeLocale, rfc822DateFormat)
import qualified Data.Text as T

parseTime = parseTimeOrError True defaultTimeLocale rfc822DateFormat . T.unpack

get url = req GET url NoReqBody bsResponse mempty

--getFeedRequest = get (https "theintercept.com" /: "feed")
getFeedRequest = get (https "www.theguardian.com" /: "world" /: "rss")
--getFeedRequest = get (https "news.ycombinator.com" /: "rss")

throwErrors Nothing  = error "error parsing feed"
throwErrors (Just f) = f

downloadFeed :: MonadIO io => io Feed
downloadFeed = throwErrors . parseFeedSource . fromStrict . responseBody <$> runReq defaultHttpConfig getFeedRequest

toFeedItem item = FeedItem
  <$> (fmap snd . getItemId $ item)
  <*> getItemTitle item
  <*> getItemLink item
  <*> (fmap parseTime . getItemDate $ item)

toFeedItems = catMaybes . map toFeedItem . getFeedItems

importItems conn = downloadFeed >>= insertFeedItems conn . toFeedItems

importFeeds = void $ connect connectInfo >>= importItems

