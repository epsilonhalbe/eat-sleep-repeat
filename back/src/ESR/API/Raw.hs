{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module ESR.API.Raw where

import Servant

type API =  "js"     :> Raw
       :<|> "static" :> Raw
       :<|> Raw

api :: Proxy API
api = Proxy

server :: Server API
server =
  serveDirectoryFileServer "../esr-front.jsexe/" :<|>
  serveDirectoryFileServer "../static/"          :<|>
  serveDirectoryFileServer "../index/"

