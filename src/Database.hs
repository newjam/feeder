{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Database (
  connectInfo, connect,
  migrate, migrateWith,
  FeedItem (..),
  insertFeedItems, insertFeedItem,
  selectFeedItems
) where


import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class


import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
--import Database.PostgreSQL.Simple.Time

import Data.Time.Clock (UTCTime)
import qualified Data.Text as T

import Data.Int

migrationDirectory = MigrationDirectory "db/migrations"
initialization = MigrationContext MigrationInitialization True
migrations = MigrationContext migrationDirectory True

--validate con = do
--  r <- runMigration $ MigrationContext (MigrationValidation (MigrationDirectory "db/migrations"))  True con
--  print r

migrateWith conn = do
  runMigration $ initialization conn
  withTransaction conn $ runMigration $ migrations conn
  return ()

connectInfo = defaultConnectInfo {
    connectDatabase = "feeder",
    connectUser = "feeder",
    connectPassword = "wat"
  }

migrate = connect connectInfo >>= migrateWith

data FeedItem = FeedItem {
    guid  :: T.Text,
    title :: T.Text,
    link  :: T.Text,
    date  :: UTCTime
  } deriving (Generic, Show, Eq)

instance ToRow FeedItem where
  toRow item = [
      toField . guid  $ item,
      toField . title $ item,
      toField . link  $ item,
      toField . date  $ item
    ]

instance FromRow FeedItem where
  fromRow = FeedItem <$> field <*> field <*> field <*> field


insertFeedItemStatement = "insert into feed_item (guid, title, link, date) values (?, ?, ?, ?) on conflict on constraint feed_item_pkey do nothing"

insertFeedItem :: Connection -> FeedItem -> IO Int64
insertFeedItem conn = execute conn insertFeedItemStatement


insertFeedItems :: Connection -> [FeedItem] -> IO Int64
insertFeedItems conn = executeMany conn insertFeedItemStatement

selectFeedItemQuery = "select guid, title, link, date from feed_item limit ? offset ?"

defaultFeedItemQueryOptions = (100 :: Integer, 0 :: Integer)

selectFeedItems :: MonadIO io => Connection -> io [FeedItem]
selectFeedItems conn = liftIO $ query conn selectFeedItemQuery defaultFeedItemQueryOptions

