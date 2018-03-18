{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}
module ESR.API where

import           Data.Text           (Text)
import           Servant
import           Servant.Auth.Server (AuthResult(Authenticated), CookieSettings,
                                      JWTSettings, Cookie, Auth, throwAll)

import qualified ESR.API.Login                       as Login
import qualified ESR.API.Raw                         as Raw

type API = (Auth '[Cookie] Login.User :> Protected) :<|> Unprotected

type Protected =  "name"  :> Get '[JSON] Text
             :<|> "email" :> Get '[JSON] Text

type Unprotected =  Login.API
               :<|> Raw.API

api :: Proxy API
api = Proxy

server :: CookieSettings -> JWTSettings -> Server API
server cs jwts =  protected
             :<|> unprotected cs jwts

protected :: AuthResult Login.User -> Server Protected
protected (Authenticated Login.User{..}) = return name :<|> return email
protected _ = throwAll err401

unprotected :: CookieSettings -> JWTSettings -> Server Unprotected
unprotected cs jwts =  Login.server cs jwts
                  :<|> Raw.server

