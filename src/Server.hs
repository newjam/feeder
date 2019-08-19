{-# LANGUAGE OverloadedStrings, DataKinds #-}

module Server (serve) where

import Download
import Database

import qualified Servant
import           Servant.HTML.Blaze
import           Text.Feed.Query
import qualified Text.Blaze.Html5   as H
import qualified Text.Blaze.Html5.Attributes   as A

import Data.Maybe(catMaybes)
import Control.Monad(forM_)

import Control.Monad.IO.Class

import qualified Data.Text as T

import qualified Network.Wai.Handler.Warp as Warp

serve = do
  putStrLn "serving on http://localhost:3000/"
  Warp.run 3000 application


type API = Servant.Get '[HTML] Homepage
type Homepage = H.Html

api :: Servant.Proxy API
api = Servant.Proxy

server :: Servant.Server API
server = renderFeed <$> liftIO (connect connectInfo >>= selectFeedItems)

application = Servant.serve api server


renderFeedItem :: FeedItem -> H.Html
renderFeedItem item = H.li $ H.a H.! (A.href . H.textValue . link $ item) $ (H.toMarkup . title $ item)

renderFeed :: [FeedItem] -> H.Html
renderFeed items = H.docTypeHtml $ do
  H.head $ do
    H.title "News"
  H.body $ do
    H.h1 $ "News"
    H.ul $ forM_ items renderFeedItem

