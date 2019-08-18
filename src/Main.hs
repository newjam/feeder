{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.HTTP.Req
import Text.Feed.Import
import Text.Feed.Query
import Data.ByteString.Lazy(fromStrict)

get url = req GET url NoReqBody bsResponse mempty

getFeedRequest = get (https "theintercept.com" /: "feed")

main :: IO ()
main = do
  response <- runReq defaultHttpConfig getFeedRequest
  let body = fromStrict . responseBody $ response
  case parseFeedSource body of
    Nothing   -> error "error reading feed"
    Just feed -> print . getFeedTitle $ feed

