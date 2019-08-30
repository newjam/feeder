{-# LANGUAGE DataKinds, TypeOperators, LambdaCase #-}
module Server (serve) where

import System.Exit

import Database

import qualified Servant
import           Servant.HTML.Blaze
import           Servant ((:<|>), (:>))
import Network.HTTP.Media ((//))

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A

import Control.Monad(forM_)
import Control.Monad.IO.Class

import qualified Network.Wai.Handler.Warp as Warp
import           Network.Wai.Logger (withStdoutLogger)

import Data.String

import Paths_feeder

import qualified Data.Text as T

import qualified Network.URI as URI

serve :: ConnectInfo -> Int -> String -> IO ()
serve connInfo port host = do
  staticDir <- liftIO $ getDataFileName "data/static"
  Database.validate connInfo >>= \case
    MigrationError e -> do
      putStrLn $ "Database invalid: " ++ e
      exitFailure
    MigrationSuccess -> do
      putStrLn $ "serving on http://localhost:" ++ show port ++ "/"
      withStdoutLogger $ \logger -> do
        let settings = Warp.setPort   port
                     . Warp.setHost   (fromString host)
                     . Warp.setLogger logger
                     $ Warp.defaultSettings
        Warp.runSettings settings (application staticDir connInfo)



type API = (Servant.Get '[HTML] Homepage)
      :<|> ("static" :> Servant.Raw)
      :<|> ("clicks" :> Servant.Header "Ping-To" T.Text :> Servant.Post '[Ping] Servant.NoContent)

type Homepage = H.Html

data Ping

instance Servant.Accept Ping where
  contentType _ = "text" // "ping"

api :: Servant.Proxy API
api = Servant.Proxy

server :: FilePath -> ConnectInfo -> Servant.Server API
server staticDir connInfo = (renderFeed <$> liftIO (connect connInfo >>= selectFeedItems))
                       Servant.:<|> Servant.serveDirectoryWebApp staticDir
                       Servant.:<|> newClick connInfo

newClick :: ConnectInfo -> Maybe T.Text -> Servant.Handler Servant.NoContent
newClick connInfo = \case
  Nothing   -> return Servant.NoContent
  Just link -> do
    conn <- liftIO . connect $ connInfo
    Database.incrementFeedItemClickCount conn link
    return Servant.NoContent

application :: FilePath -> ConnectInfo -> Servant.Application
application staticDir connInfo = do
  Servant.serve api (server staticDir connInfo)

renderFeedItem :: FeedItem -> H.Html
renderFeedItem x = H.li ! A.class_ "feed-items__item" $ do
  H.span ! A.class_ "feed-items__item-date" $ H.toMarkup . show . Database.date $ x
  case URI.uriAuthority . Database.link $ x of
    Just auth -> H.span ! A.class_ "feed-items__item-domain" $ H.toMarkup . URI.uriRegName $ auth
    Nothing   -> return ()
  H.a ! A.class_ "feed-items__item-link"
      ! (A.href . H.stringValue . show . Database.link $ x)
      ! A.ping "clicks"
      $ (H.toMarkup . Database.title $ x)

renderFeed :: [FeedItem] -> H.Html
renderFeed items = H.docTypeHtml $ do
  H.head $ do
    H.title "News"
    H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "static/feeder.css"
  H.body $ do
    H.h1 $ "News"
    H.ul ! A.class_ "feed-items" $ forM_ items renderFeedItem
    H.script ! A.type_ "application/javascript" ! A.src "https://moment.github.io/luxon/global/luxon.min.js" $ return ()
    H.script ! A.type_ "application/javascript" ! A.src "static/feeder.js" $ return ()
