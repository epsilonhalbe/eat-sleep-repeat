{-# LANGUAGE OverloadedStrings #-}
module ESR.Login ( Model(..)
                 , Config(..)
                 , makeModel
                 , widget
                 ) where

import           Data.Bifunctor                    (second)
import           Data.Maybe                        (fromMaybe)
import           Data.Monoid                       ((<>))
import           Data.Text                         as T

import           Reflex.Dom

import           ESR.Login.Types                   (Login(..))
import           ESR.Login.Internal
import           MDC.Button                        as MDC
import           MDC.TextField                     as MDC



widget :: MonadWidget t m => Model t -> m (Config t)
widget model =
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


fetchEmail :: MonadWidget t m => Model t -> m (Config t, Event t (Maybe Text))
fetchEmail model = do
  click <- MDC.buttonDynAttr (disabled <$> xsrf model) $ text "Email"
  second (fmap _xhrResponse_responseText) <$> fetch "/email" model click

fetchName :: MonadWidget t m => Model t -> m (Config t, Event t (Maybe Text))
fetchName model = do
  click <- MDC.buttonDynAttr (disabled <$> xsrf model) $ text "Name"
  second (fmap _xhrResponse_responseText) <$> fetch "/name" model click

loginBox :: MonadWidget t m => m (Config t)
loginBox = elAttr "div" ( "class" =: "mdc-card"
                       <> "style" =: "max-width:30em;margin:auto;padding:2em;"
                        ) $ do
  (u, p) <- elClass "div" "mdc-card__card-media" $
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
                    MDC.buttonAttr ("aria-label" =: "submit") $ text "Submit"
  let login = Login <$> _textInput_value u
                    <*> _textInput_value p
      submit = leftmost [ submitButton
                        , keypress Enter u
                        , keypress Enter p
                        ]
      submitLogin = postJson "/login" <$> tagPromptlyDyn login submit
  resp <- performRequestAsync submitLogin
  el "br" blank
  Config <$> getXsrfCookie resp


