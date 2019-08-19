{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import qualified Database
import qualified Server
import qualified Download

data Command = Migrate | Serve | Import

commands = subparser (migrateCommand <> serveCommand <> importCommand)

migrateCommand = command "migrate" (info
    (pure Migrate)
    (progDesc "migrate application database")
  )

serveCommand = command "serve" (info
    (pure Serve)
    (progDesc "serve web application")
  )

importCommand = command "import" (info
    (pure Import)
    (progDesc "import feeds to database")
  )

main = execParser (info (commands <**> helper) fullDesc) >>= run

run Migrate = Database.migrate
run Serve   = Server.serve
run Import  = Download.importFeeds
