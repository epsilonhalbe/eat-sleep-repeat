{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad          (void)
import           Data.Monoid            ((<>))
import           Data.Text
import           Data.Map               (Map)
import           GHCJS.DOM.Element      (toElement)
import           GHCJS.DOM.Types        ( HTMLInputElement, MonadJSM, liftJSM
                                        , JSVal, unElement)
import qualified GHCJS.DOM.Types        as GDT
import qualified GHCJS.DOM.JSFFI.Generated.Element as FFI
--import qualified JSDOM.Generated
import           Reflex.Dom
import           Lens.Micro

data Login = Login { username :: Text
                   , password :: Text }
           deriving (Eq, Show)



foreign import javascript unsafe
    "new mdc.textField.MDCTextField($1);"
    js_newTextField :: JSVal -> IO ()

newTextField :: MonadWidget t m => GDT.Element -> m ()
newTextField el = do
  let jsel = GDT.unElement $ toElement el
  pb <- getPostBuild
  performEvent_ $ liftIO (js_newTextField jsel) <$ pb

main :: IO ()
main = mainWidgetInElementById "esr" main'

main' :: MonadWidget t m => m ()
main' = void $
  elClass "div" "mdc-typography" $ do
    elClass "h2" "mdc-typography--display2" $ text "Hello, Material Components!"
    loginBox

loginBox :: MonadWidget t m => m (Event t Login)
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
  pure $ tagPromptlyDyn login submit

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
      FFI.setAttribute (_textInput_element ti) ("class" :: String) ("mdc-text-field__input" :: String)
      return ti
  newTextField $ _element_raw $ fst this
  el "br" blank
  return this



