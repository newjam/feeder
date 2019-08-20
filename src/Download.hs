module Download (importFeed) where

import Database

import Network.Wreq
import Control.Lens

import Text.Feed.Types
import Text.Feed.Import
import Text.Feed.Query

import Control.Monad (join)
import Data.Maybe (catMaybes)

downloadFeed :: String -> IO Feed
downloadFeed url = do
  let acceptableTypes = ["application/atom+xml", "application/atom+rss"]
  let opts = defaults & header "Accept" .~ acceptableTypes
  response <- getWith opts url
  case parseFeedSource $ response ^. responseBody of
    Nothing   -> error "error parsing feed"
    Just feed -> return feed

downloadFeedItems url = toFeedItems <$> downloadFeed url

toFeedItem item = FeedItem
  <$> (fmap snd . getItemId $ item)
  <*> getItemTitle item
  <*> getItemLink item
  <*> (join . getItemPublishDate $ item)

toFeedItems = catMaybes . map toFeedItem . getFeedItems

report url n = putStrLn ("Imported " ++ (show n) ++ " items from " ++ url)

importFeed connectInfo url = do
  items <- downloadFeedItems url
  conn  <- connect connectInfo
  count <- insertFeedItems conn items
  report url count
