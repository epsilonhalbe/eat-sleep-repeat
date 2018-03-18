{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module ESR.API.Login where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.ByteString.Char8  (unpack)
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import           Servant
import           Servant.Auth.Server    (CookieSettings, FromJWT, JWTSettings,
                                         SetCookie, ToJWT, makeCsrfCookie,
                                         makeSessionCookie)
import           Web.Cookie             (setCookieValue)


type XSRFHeader content = Headers '[ Header "XSRF-TOKEN" String
                                   , Header "Set-Cookie" SetCookie] content

type API = "login" :> ReqBody       '[JSON] Login
                   :> PostNoContent '[JSON] (XSRFHeader NoContent)

data Login = Login { username :: Text, password :: Text }
   deriving (Eq, Show, Read, Generic)

instance ToJSON   Login
instance FromJSON Login

server :: CookieSettings -> JWTSettings -> Login -> Handler (XSRFHeader NoContent)
server cookieSettings jwtSettings (Login "test" "test") = do             -- TODO: Login
  let usr = User "name" "mail"                                           -- TODO: User
  (csrf, session) <- liftIO $
                (,) <$> makeCsrfCookie cookieSettings
                    <*> makeSessionCookie cookieSettings jwtSettings usr

  maybe (throwError err401) (pure .   addHeader (unpack $ setCookieValue csrf)
                                  . (`addHeader` NoContent)) session
server _ _ _ = throwError err401

data User = User { name :: Text, email :: Text }
   deriving (Eq, Show, Read, Generic)

instance ToJSON   User
instance FromJSON User

instance ToJWT   User
instance FromJWT User
