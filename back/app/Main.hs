module Main where

import           Network.Wai.Handler.Warp    (defaultSettings, setPort)
import           Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import           Network.Wai.Middleware.Cors (simpleCors)
import           Servant                     (Context ((:.), EmptyContext),
                                              serveWithContext)
import           Servant.Auth.Server         (defaultCookieSettings,
                                              defaultJWTSettings, generateKey, cookieIsSecure, IsSecure(..))

import           ESR.API                     (api, server)

port :: Int
port = 8443

main :: IO ()
main = do
  myKey <- generateKey
  let jwtCfg = defaultJWTSettings myKey
      cfg    = defaultCookieSettings :. jwtCfg :. EmptyContext
  putStrLn $ "Started cookie server on localhost:" ++ show port
  runTLS (tlsSettings "./certs/certificate.pem" "./certs/key.pem")
         (setPort port defaultSettings)
           $ simpleCors
           $ serveWithContext api cfg (server defaultCookieSettings jwtCfg)

