module Download (importFeed, ImportResult(..)) where

import Database

import Network.Wreq
import Control.Lens

import Text.Feed.Types
import Text.Feed.Import
import Text.Feed.Query

import Control.Monad (join)
import Data.Maybe (catMaybes)

import Data.Int (Int64)

import qualified Data.Text as T
import qualified Network.URI as URI

downloadFeed :: String -> IO Feed
downloadFeed url = do
  let acceptableTypes = ["application/atom+xml", "application/atom+rss"]
  let opts = defaults & header "Accept" .~ acceptableTypes
  response <- getWith opts url
  case parseFeedSource $ response ^. responseBody of
    Nothing   -> error "error parsing feed"
    Just feed -> return feed

toFeedItem url item = FeedItem
  <$> pure url
  <*> (fmap snd . getItemId $ item)
  <*> getItemTitle item
  <*> (getItemLink item >>= URI.parseURI . T.unpack)
  <*> (join . getItemPublishDate $ item)

toFeedItems url = catMaybes . map (toFeedItem url) . getFeedItems

data ImportResult = ImportResult Int64

importFeed :: ConnectInfo -> URI.URI -> IO ImportResult
importFeed connectInfo url = do
  feed  <- downloadFeed (show url)
  conn  <- connect connectInfo
  let items = toFeedItems url feed
  insertFeed conn url (getFeedTitle feed)
  count <- insertFeedItems conn items
  return $ ImportResult count
