{-# LANGUAGE DataKinds, TypeOperators, LambdaCase #-}
module Server (serve) where

import System.Exit

import Database

import qualified Servant
import           Servant.HTML.Blaze

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes

import Control.Monad(forM_)

import Control.Monad.IO.Class

import qualified Network.Wai.Handler.Warp as Warp

import Paths_feeder

serve connInfo port = do
  staticDir <- liftIO $ getDataFileName "data/static"
  Database.validate connInfo >>= \case
    MigrationError e -> do
      putStrLn $ "Database invalid: " ++ e
      exitFailure
    MigrationSuccess -> do
      putStrLn $ "serving on http://localhost:" ++ show port ++ "/"
      Warp.run port (application staticDir connInfo)

type API = (Servant.Get '[HTML] Homepage)
      Servant.:<|> ("static" Servant.:> Servant.Raw)
type Homepage = Html

api :: Servant.Proxy API
api = Servant.Proxy


server :: FilePath -> ConnectInfo -> Servant.Server API
server staticDir connInfo = (renderFeed <$> liftIO (connect connInfo >>= selectFeedItems)) Servant.:<|> Servant.serveDirectoryWebApp staticDir

application staticDir connInfo = do
  Servant.serve api (server staticDir connInfo)

renderFeedItem :: FeedItem -> Html
renderFeedItem item = li $ a ! (href . textValue . Database.link $ item) $ (toMarkup . Database.title $ item)

renderFeed :: [FeedItem] -> Html
renderFeed items = docTypeHtml $ do
  H.head $ do
    H.title "News"
    H.link ! rel "stylesheet" ! type_ "text/css" ! href "static/feeder.css"
  body $ do
    h1 $ "News"
    ul $ forM_ items renderFeedItem

