{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Options.Applicative
import Data.Semigroup ((<>))

import qualified Database
import qualified Server
import qualified Download

import System.Exit

import Database.PostgreSQL.Simple.URL (parseDatabaseUrl)
import Database.PostgreSQL.Simple (ConnectInfo)

data Command = Migrate ConnectInfo | Serve ConnectInfo Int | Import ConnectInfo String

commands = subparser (migrateCommand <> serveCommand <> importCommand)

migrateCommand = command "migrate" (info
    (migrateOptions <**> helper)
    (progDesc "migrate application database")
  )

serveCommand = command "serve" (info
    (serveOptions <**> helper)
    (progDesc "serve web application")
  )

importCommand = command "import" (info
    (importOptions <**> helper)
    (progDesc "import Atom or RSS feed from URL")
  )

migrateOptions = Migrate <$> databaseUrl
serveOptions   = Serve   <$> databaseUrl <*> port
importOptions  = Import  <$> databaseUrl <*> importUrl

port = option auto (
     long "port"
  <> help "Port to serve on"
  <> showDefault
  <> value 3000
  <> metavar "PORT")

importUrl = argument str (metavar "FEED_URL")

databaseUrl = option (maybeReader parseDatabaseUrl)
          ( long "database-url"
         <> value Database.defaultConnectInfo
         <> metavar "DB_URL"
         <> help "url of postgres database")

parser = info (commands <**> helper) fullDesc

main = execParser parser >>= \case
  Migrate connInfo      -> Database.migrate    connInfo >>= \case
    Database.MigrationSuccess -> exitSuccess
    Database.MigrationError _ -> exitFailure
  Serve   connInfo port -> Server.serve        connInfo port
  Import  connInfo url  -> Download.importFeed connInfo url
