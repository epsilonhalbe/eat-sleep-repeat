{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Prelude

import           Control.Arrow                     ((&&&))
import           Control.Monad                     (void)
import           Control.Monad.IO.Class            (MonadIO, liftIO)
import           Data.Aeson
import           Data.Aeson.Text
import           Data.Bifunctor
import           Data.Map                          (Map)
import qualified Data.Map                          as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text                         as T
import           Data.Text.Lazy                    (toStrict)
import           GHC.Generics                      (Generic)

import           GHCJS.DOM.Element                 (toElement)
import qualified GHCJS.DOM.JSFFI.Generated.Element as FFI
import qualified GHCJS.DOM.Types                   as GDT
import           GHCJS.DOM.Types                   (JSVal, unElement, IsElement)

import           Language.Javascript.JSaddle
import           Lens.Micro
import           Network.URI.Encode                as E
import           Reflex.Dom
import           Reflex.Network

import           ESR.Login.Types                   (Login(..))

type XSRF = Text

newtype Model t  = Model  { xsrf :: Dynamic t (Maybe XSRF) }
newtype Config t = Config { newXSRF :: Event t XSRF }

instance Reflex t => Monoid (Config t) where
  mappend (Config x) (Config y) = Config $ leftmost [x,y]
  mconcat confs = Config $ leftmost $ newXSRF <$> confs
  mempty = Config never

makeModel :: (Reflex t, MonadHold t m) => Config t -> m (Model t)
makeModel c = Model <$> holdDyn Nothing (Just <$> newXSRF c)

main :: IO ()
main = mainWidgetInElementById "esr" $ mdo
  model <- makeModel conf
  confEvent <- networkView $ pure (ui' model)
  conf <- Config <$> switchHold never (newXSRF <$> confEvent)
  pure ()

ui' :: MonadWidget t m => Model t -> m (Config t)
ui' model =
  elClass "div" "mdc-typography" $ do
    elClass "h2" "mdc-typography--display2" $ text "Hello, Material Components!"
    confLogin <- loginBox
    dynText $ fromMaybe "nothing" <$> xsrf model
    el "br" blank

    (confEmail, email) <- fetchEmail model
    dynText =<< fmap (maybe "<^_^>" ("email: " <>)) <$> holdDyn Nothing email

    el "br" blank
    (confName , name)  <- fetchName  model
    dynText =<< fmap (maybe "<^_^>" ("name: " <>)) <$> holdDyn Nothing name

    el "br" blank
    pure $ confLogin <> confEmail <> confName

disabled :: Maybe a -> Map Text Text
disabled Nothing = "disabled" =: "true"
disabled _       = mempty

fetch :: MonadWidget t m => Text -> Model t -> Event t a -> m (Config t, Event t XhrResponse)
fetch route Model{..} trigger = do
  let withXSRF = maybe mempty ("X-XSRF-TOKEN" =:) <$> tagPromptlyDyn xsrf trigger
      get x = xhrRequest "GET" route def{ _xhrRequestConfig_headers = x }
  resp <- performRequestAsync (get <$> withXSRF)
  newXSRF <- getXsrfCookie resp
  pure (Config{..}, resp)

fetchEmail :: MonadWidget t m => Model t -> m (Config t, Event t (Maybe Text))
fetchEmail model = do
  click <- mdcDynButton (disabled <$> xsrf model) $ text "Email"
  second (fmap _xhrResponse_responseText) <$> fetch "/email" model click

fetchName :: MonadWidget t m => Model t -> m (Config t, Event t (Maybe Text))
fetchName model = do
  click <- mdcDynButton (disabled <$> xsrf model) $ text "Name"
  second (fmap _xhrResponse_responseText) <$> fetch "/name" model click

loginBox :: MonadWidget t m => m (Config t)
loginBox = elAttr "div" ( "class" =: "mdc-card"
                       <> "style" =: "max-width:30em;margin:auto;padding:2em;"
                        ) $ do
  (username, password) <- elClass "div" "mdc-card__card-media" $
    elClass "div" "mdc-card__card-media-content" $ do
      elClass "h1" "mdc-typography--display1" $ text "Login"
      u <- textfield def{ _textInputConfig_attributes = pure ("aria-label" =: "login")}
                     "username or email"
      p <- textfield def{ _textInputConfig_attributes = pure ("aria-label" =: "password")
                        , _textInputConfig_inputType = "password" }
                     "password"
      pure (u, p)
  submitButton <- elClass "div" "mdc-card__actions" $
                    elClass "div" "mdc-card__action-buttons" $
                    mdcButton ("aria-label" =: "submit") $ text "Submit"
  let login = Login <$> _textInput_value username
                    <*> _textInput_value password
      submit = leftmost [ submitButton
                        , keypress Enter username
                        , keypress Enter password
                        ]
      submitLogin = postJson' "/login" <$> tagPromptlyDyn login submit
  resp <- performRequestAsync submitLogin
  el "br" blank
  Config <$> getXsrfCookie resp


postJson' :: (ToJSON a) => Text -> a -> XhrRequest Text
postJson' url a =
  XhrRequest "POST" url $ def { _xhrRequestConfig_headers         = headerUrlEnc
                              , _xhrRequestConfig_sendData        = body
                              , _xhrRequestConfig_responseHeaders = AllHeaders
                              }
  where body = toStrict $ encodeToLazyText a
        headerUrlEnc = "Content-type" =: "application/json"



mdcDynButton :: (PostBuild t m, DomBuilder t m)
             => Dynamic t (Map Text Text) -> m a -> m (Event t ())
mdcDynButton attrs = fmap (domEvent Click . fst)
                   . elDynAttr' "button" (attrs' <$> attrs)
  where attrs' attrs = "class"              =: "mdc-button mdc-card__action mdc-card__action--button"
                    <> "data-mdc-auto-init" =: "MDCRipple"
                    <> attrs

mdcButton :: (PostBuild t m, DomBuilder t m)
          => Map Text Text -> m a -> m (Event t ())
mdcButton attrs = fmap (domEvent Click . fst)
                . elAttr' "button" attrs'
  where attrs' = "class"              =: "mdc-button mdc-card__action mdc-card__action--button"
              <> "data-mdc-auto-init" =: "MDCRipple"
              <> attrs

mdcDynFABIcon :: (PostBuild t m, DomBuilder t m) => Dynamic t Text -> m (Event t ())
mdcDynFABIcon = fmap (domEvent Click . fst)
        . elAttr' "button" attrs
        . elClass "span" "mdc-fab__icon"
        . dynText
  where attrs = "class"              =: "mdc-fab material-icons"
             <> "aria-label"         =: "switch"
             <> "data-mdc-auto-init" =: "MDCRipple"

textfield :: MonadWidget t m => TextInputConfig t -> Text -> m (TextInput t)
textfield conf label = snd <$> textfield' conf label

textfield' :: MonadWidget t m => TextInputConfig t -> Text -> m (Element EventResult (DomBuilderSpace m) t, TextInput t)
textfield' conf label = do
  this <- elClass' "div" "mdc-text-field" $ do
      elClass "label"  "mdc-text-field__label" $ text label
      ti <- textInput conf
      elClass "div" "mdc-line-ripple" blank
      FFI.setAttribute (_textInput_element ti) ("class" :: Text) ("mdc-text-field__input" :: Text)
      return ti
  newTextField $ _element_raw $ fst this
  el "br" blank
  return this

