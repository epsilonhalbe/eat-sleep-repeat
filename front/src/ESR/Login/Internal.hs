{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ESR.Login.Internal where

import           Prelude                           hiding ((!!))
import           Control.Monad                     ((<=<))
import           Data.Map                          (Map)
import           Data.Text                         (Text)

import           Language.Javascript.JSaddle
import           Reflex.Dom

type XSRF = Text

newtype Model t  = Model  { xsrf :: Dynamic t (Maybe XSRF) }
newtype Config t = Config { newXSRF :: Event t XSRF }

instance Reflex t => Monoid (Config t) where
  mappend (Config x) (Config y) = Config $ leftmost [x,y]
  mconcat confs = Config $ leftmost $ newXSRF <$> confs
  mempty = Config never

makeModel :: (Reflex t, MonadHold t m) => Config t -> m (Model t)
makeModel c = Model <$> holdDyn Nothing (Just <$> newXSRF c)

fetch :: MonadWidget t m
      => Text -> Model t -> Event t a -> m (Config t, Event t XhrResponse)
fetch route Model{..} trigger = do
  let withXSRF = maybe mempty ("X-XSRF-TOKEN" =:) <$> tagPromptlyDyn xsrf trigger
      get x = xhrRequest "GET" route def{ _xhrRequestConfig_headers = x }
  resp <- performRequestAsync (get <$> withXSRF)
  newXSRF <- getXsrfCookie resp
  pure (Config{..}, resp)


-- helper functions

disabled :: Maybe a -> Map Text Text
disabled Nothing = "disabled" =: "true"
disabled _       = mempty

getXsrfCookie :: MonadWidget t m => Event t XhrResponse -> m (Event t XSRF)
getXsrfCookie resp = fmapMaybe id <$> performEvent (xsrf <$ resp)
  where xsrf :: MonadJSM m => m (Maybe XSRF)
        xsrf = liftJSM $ do
          doc <- jsg ("document" :: Text)
          cookie <- doc ! ("cookie" :: Text)
          xsrfMatchM <-  maybeNullOrUndefined
                     =<< (cookie # ("match" :: Text) $ ("^XSRF-TOKEN=([^=]+=)" :: Text))
          traverse (valToText <=< (!! 1)) xsrfMatchM

