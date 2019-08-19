{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import qualified Database
import qualified Server
import qualified Download

data Command = Migrate | Serve Int | Import String

commands = subparser (migrateCommand <> serveCommand <> importCommand)

migrateCommand = command "migrate" (info
    (pure Migrate)
    (progDesc "migrate application database")
  )

serveCommand = command "serve" (info
    serveOptions
    (progDesc "serve web application")
  )

serveOptions = Serve <$> (option auto
          ( long "port"
         <> help "Port to serve on"
         <> showDefault
         <> value 3000
         <> metavar "PORT"))

importCommand = command "import" (info
    importOptions
    (progDesc "import Atom or RSS feed from URL")
  )

importUrlOption = argument str (metavar "URL")

importOptions = Import <$> importUrlOption

main = execParser (info (commands <**> helper) fullDesc) >>= run

run Migrate      = Database.migrate
run (Serve port) = Server.serve port
run (Import url) = Download.importFeed url
