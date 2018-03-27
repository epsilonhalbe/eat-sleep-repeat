{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module ESR.API.Login ( API
                     , server
                     , User(..)
                     ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.ByteString.Char8  (unpack)
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import           Servant
import           Servant.Auth.Server    (CookieSettings, JWTSettings,
                                         SetCookie, makeCsrfCookie,
                                         makeSessionCookie)
import           Web.Cookie             (setCookieValue)


import           ESR.Login.Types        (Login(..), User(..))

type XSRFHeader content = Headers '[ Header "XSRF-TOKEN" String
                                   , Header "Set-Cookie" SetCookie] content

type API = "login" :> ReqBody       '[JSON] Login
                   :> PostNoContent '[JSON] (XSRFHeader NoContent)


server :: CookieSettings -> JWTSettings -> Login -> Handler (XSRFHeader NoContent)
server cookieSettings jwtSettings (Login "test" "test") = do             -- TODO: Login
  let usr = User "name" "mail"                      -- TODO: User
  (csrf, session) <- liftIO $
                (,) <$> makeCsrfCookie cookieSettings
                    <*> makeSessionCookie cookieSettings jwtSettings usr

  maybe (throwError err401) (pure .   addHeader (unpack $ setCookieValue csrf)
                                  . (`addHeader` NoContent)) session
server _ _ _ = throwError err401

