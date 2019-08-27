{-# LANGUAGE DataKinds, TypeOperators, LambdaCase #-}
module Server (serve) where

import System.Exit

import Database

import qualified Servant
import           Servant.HTML.Blaze

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A

import Control.Monad(forM_)
import Control.Monad.IO.Class

import qualified Network.Wai.Handler.Warp as Warp

import Paths_feeder

import qualified Network.URI as URI

serve :: ConnectInfo -> Int -> IO ()
serve connInfo port = do
  staticDir <- liftIO $ getDataFileName "data/static"
  Database.validate connInfo >>= \case
    MigrationError e -> do
      putStrLn $ "Database invalid: " ++ e
      exitFailure
    MigrationSuccess -> do
      putStrLn $ "serving on http://localhost:" ++ show port ++ "/"
      let settings = Warp.setPort port
                   . Warp.setHost "127.0.0.1"
                   $ Warp.defaultSettings
      Warp.runSettings settings (application staticDir connInfo)

type API = (Servant.Get '[HTML] Homepage)
      Servant.:<|> ("static" Servant.:> Servant.Raw)
type Homepage = H.Html

api :: Servant.Proxy API
api = Servant.Proxy

server :: FilePath -> ConnectInfo -> Servant.Server API
server staticDir connInfo = (renderFeed <$> liftIO (connect connInfo >>= selectFeedItems)) Servant.:<|> Servant.serveDirectoryWebApp staticDir

application :: FilePath -> ConnectInfo -> Servant.Application
application staticDir connInfo = do
  Servant.serve api (server staticDir connInfo)

renderFeedItem :: FeedItem -> H.Html
renderFeedItem x = H.li ! A.class_ "feed-items__item" $ do
  H.span ! A.class_ "feed-items__item-date" $ H.toMarkup . show . Database.date $ x
  case URI.uriAuthority . Database.link $ x of
    Just auth -> H.span ! A.class_ "feed-items__item-domain" $ H.toMarkup . URI.uriRegName $ auth
    Nothing   -> return ()
  H.a ! A.class_ "feed-items__item-link" ! (A.href . H.stringValue . show . Database.link $ x) $ (H.toMarkup . Database.title $ x)

renderFeed :: [FeedItem] -> H.Html
renderFeed items = H.docTypeHtml $ do
  H.head $ do
    H.title "News"
    H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "static/feeder.css"
  H.body $ do
    H.h1 $ "News"
    H.ul ! A.class_ "feed-items" $ forM_ items renderFeedItem

