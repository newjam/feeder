{-# LANGUAGE DataKinds #-}
module Server (serve) where

import Database

import qualified Servant
import           Servant.HTML.Blaze

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes

import Control.Monad(forM_)

import Control.Monad.IO.Class

import qualified Network.Wai.Handler.Warp as Warp

serve connInfo port = do
  putStrLn $ "serving on http://localhost:" ++ show port ++ "/"
  Warp.run port (application connInfo)

type API = Servant.Get '[HTML] Homepage
type Homepage = Html

api :: Servant.Proxy API
api = Servant.Proxy

server :: ConnectInfo -> Servant.Server API
server connInfo = renderFeed <$> liftIO (connect connInfo >>= selectFeedItems)

application connInfo = Servant.serve api (server connInfo)

renderFeedItem :: FeedItem -> Html
renderFeedItem item = li $ a ! (href . textValue . Database.link $ item) $ (toMarkup . Database.title $ item)

renderFeed :: [FeedItem] -> Html
renderFeed items = docTypeHtml $ do
  H.head $ do
    H.title "News"
  body $ do
    h1 $ "News"
    ul $ forM_ items renderFeedItem

