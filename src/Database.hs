module Database (
  connect, defaultConnectInfo,
  ConnectInfo(..), MigrationResult(..),
  migrate, migrateWith,
  validate, validateWith,
  FeedItem (..),
  insertFeed,
  insertFeedItems, insertFeedItem,
  selectFeedItems
) where

import Control.Monad.IO.Class

import Paths_feeder

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField

import Data.Time.Clock (UTCTime)
import qualified Data.Text as T

import Data.Monoid ((<>))

import Data.Int

migrateWith :: Connection -> IO (MigrationResult String)
migrateWith conn = do
  path <- getDataFileName "data/migrations"
  let migration = MigrationInitialization <> MigrationDirectory path
  let migrationContext = MigrationContext migration True conn
  withTransaction conn $ runMigration migrationContext

validateWith :: Connection -> IO (MigrationResult String)
validateWith conn = do
  path <- getDataFileName "data/migrations"
  let checkInit    = MigrationValidation MigrationInitialization
  let checkMigrate = MigrationValidation (MigrationDirectory path)
  runMigration $ MigrationContext (checkInit <> checkMigrate) False conn

validate :: ConnectInfo -> IO (MigrationResult String)
validate connectInfo = connect connectInfo >>= validateWith


migrate :: ConnectInfo -> IO (MigrationResult String)
migrate connectInfo = connect connectInfo >>= migrateWith

data FeedItem = FeedItem {
    feed  :: T.Text,
    guid  :: T.Text,
    title :: T.Text,
    link  :: T.Text,
    date  :: UTCTime
  } deriving (Show, Eq)

instance ToRow FeedItem where
  toRow item = [
      toField . feed  $ item,
      toField . guid  $ item,
      toField . title $ item,
      toField . link  $ item,
      toField . date  $ item
    ]

instance FromRow FeedItem where
  fromRow = FeedItem <$> field <*> field <*> field <*> field <*> field

insertFeedItemStatement :: Query
insertFeedItemStatement = "insert into feed_item (feed, guid, title, link, date) values (?, ?, ?, ?, ?) on conflict on constraint feed_item_pkey do nothing"

insertFeedStatement :: Query
insertFeedStatement = "insert into feed (link, title) values (?, ?) on conflict on constraint feed_pkey do nothing"

insertFeed :: Connection -> T.Text -> T.Text -> IO Int64
insertFeed conn url title = execute conn insertFeedStatement (url, title)

insertFeedItem :: Connection -> FeedItem -> IO Int64
insertFeedItem conn = execute conn insertFeedItemStatement

insertFeedItems :: Connection -> [FeedItem] -> IO Int64
insertFeedItems conn = executeMany conn insertFeedItemStatement

selectFeedItemQuery :: Query
selectFeedItemQuery = "select guid, title, link, date from feed_item order by date desc limit ? offset ?"

selectFeedItems :: MonadIO io => Connection -> io [FeedItem]
selectFeedItems conn = liftIO $ query conn selectFeedItemQuery params where
  params = (100 :: Integer, 0 :: Integer)

