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
import           Servant                hiding (NotSecure)
import           Servant.Auth.Server    (CookieSettings(..), JWTSettings,
                                         SetCookie, makeCsrfCookie,
                                         makeSessionCookie, cookieIsSecure, IsSecure(..))
import           Web.Cookie             (setCookieValue)

import           ESR.Login.Types        (Login(..), User(..))

type API = "login" :> ReqBody       '[JSON] Login
                   :> PostNoContent '[JSON] (WithXSRF NoContent)

type WithXSRF content = Headers '[ Header "Set-Cookie" SetCookie
                                 , Header "Set-Cookie" SetCookie] content

server :: CookieSettings -> JWTSettings -> Login -> Handler (WithXSRF NoContent)
server cs js (Login "test" "test") = do             -- TODO: Login
  let usr = User "name" "mail"                      -- TODO: User
  (csrf, mSession) <- liftIO $
     (,) <$> makeCsrfCookie    cs
         <*> makeSessionCookie cs js usr
  case mSession of
    Just session -> pure $ addHeader csrf
                         $ addHeader session NoContent
    Nothing      -> throwError err401
server _ _ _ = throwError err401

