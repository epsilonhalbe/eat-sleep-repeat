{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad          (void)
import           Data.Monoid            ((<>))
import           Data.Text
import           GHCJS.DOM.Element      (toElement)
import           GHCJS.DOM.Types        ( HTMLInputElement, MonadJSM, liftJSM
                                        , JSVal, unElement)
import qualified GHCJS.DOM.Types        as GDT
import qualified GHCJS.DOM.JSFFI.Generated.Element as FFI
--import qualified JSDOM.Generated
import           Reflex.Dom
import           Lens.Micro

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
    blank

button' :: (PostBuild t m, DomBuilder t m) => Dynamic t Text -> m (Event t ())
button' = fmap (domEvent Click . fst)
        . elAttr' "button" attrs
        . elClass "span" "mdc-fab__icon"
        . dynText
  where attrs = "class"              =: "mdc-fab material-icons"
             <> "aria-label"         =: "switch"
             <> "data-mdc-auto-init" =: "MDCRipple"

textfield :: MonadWidget t m => Text -> m (TextInput t)
textfield = fmap snd . textfield'

textfield' :: MonadWidget t m => TextInputConfig t -> Text -> m (Element EventResult (DomBuilderSpace m) t, TextInput t)
textfield' _ str = do
  this <- elAttr' "div" textFieldAttrs $ do
      elClass "label"  "mdc-text-field__label" $ text str
      ti <- textInput def {_textInputConfig_attributes = textFieldAttrs}
      elClass "div" "mdc-line-ripple" blank
      FFI.setAttribute (_textInput_element ti) ("class" :: String) ("mdc-text-field__input" :: String)
      return ti
  newTextField $ _element_raw $ fst this
  el "br" blank
  return this
  where
    textFieldAttrs = pure $ "class"      =: "mdc-text-field mdc-text-field__input"
                         <> "aria-label" =: "loginbox"



