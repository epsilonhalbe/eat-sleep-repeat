{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
module ESR.API.Login ( API
                     , server
                     , User(..)
                     ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.ByteString.Builder(toLazyByteString)
import           Data.ByteString.Char8  (ByteString, unpack)
import           Data.ByteString.Lazy   (toStrict)
import           Data.CaseInsensitive   (CI)
import           Data.Text              (Text)
import           Data.Default           (def)
import           GHC.Generics           (Generic)
import           Data.Monoid            (mempty)
import           Servant                hiding (NotSecure)
import           Servant.Auth.Server    (CookieSettings(..), JWTSettings
                                        ,makeCsrfCookie, makeSessionCookie
                                        ,cookieIsSecure, IsSecure(..)
                                        ,acceptLogin)
import           Web.Cookie             (SetCookie(..), renderSetCookie)

import           ESR.Login.Types        (Login(..), User(..))

type API = "login" :> ReqBody       '[JSON] Login
                   :> PostNoContent '[JSON] (WithXSRF NoContent)

type WithXSRF content = Headers '[ Header "Set-Cookie" SetCookie
                                 , Header "Set-Cookie" SetCookie] content

server :: CookieSettings -> JWTSettings -> Login -> Handler (WithXSRF NoContent)
server cs js (Login "test" "test") = do             -- TODO: Login
  let usr = User "name" "mail"                      -- TODO: User
  mApplyCookies <- liftIO $ acceptLogin cs js usr
  case mApplyCookies of
    Just applyCookies -> pure $ applyCookies NoContent
    Nothing           -> throwError err401{errHeaders = clearCookies cs}
server cs _ _ = throwError err401{errHeaders = clearCookies cs}


clearCookies cs = [("Set-Cookie", toStrict . toLazyByteString . renderSetCookie $ f cs)
                  | f <- [clearCsrfCookie, clearSessionCookie]
                  ]

clearCsrfCookie :: CookieSettings -> SetCookie
clearCsrfCookie cs = def
    { setCookieName    = xsrfCookieName cs
    , setCookieValue   = mempty
    , setCookieMaxAge  = cookieMaxAge cs
    , setCookieExpires = cookieExpires cs
    , setCookiePath    = xsrfCookiePath cs
    , setCookieSecure  = False
    }

clearSessionCookie :: CookieSettings -> SetCookie
clearSessionCookie cs = def
    { setCookieName     = sessionCookieName cs
    , setCookieValue    = mempty
    , setCookieHttpOnly = True
    , setCookieMaxAge   = cookieMaxAge cs
    , setCookieExpires  = cookieExpires cs
    , setCookiePath     = cookiePath cs
    , setCookieSecure   = False
    }

