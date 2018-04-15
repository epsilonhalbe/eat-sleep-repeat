{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MDC.TextField  where

import           Control.Monad                     (void)
import           Control.Monad.IO.Class            (liftIO)
import           Data.Text                         (Text)

import           GHCJS.DOM.Element                 (toElement)
import qualified GHCJS.DOM.JSFFI.Generated.Element as FFI
import qualified GHCJS.DOM.Types                   as GDT
import           GHCJS.DOM.Types                   (JSVal)

import           Language.Javascript.JSaddle
import           Reflex.Dom

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
  newTextField' $ fst this
  el "br" blank
  return this

newTextField :: MonadWidget t m => GDT.Element -> m ()
newTextField e = do
  pb <- getPostBuild
  let makeNewTextField = liftIO (js_newTextField $ GDT.unElement $ toElement e)
  performEvent_ $ makeNewTextField <$ pb

foreign import javascript unsafe
    "new mdc.textField.MDCTextField($1);"
    js_newTextField :: JSVal -> IO ()

newTextField' :: (GDT.IsElement (RawElement d), Reflex t, MonadJSM m)
              => Element e d t-> m ()
newTextField' x = liftJSM $ do
  let x' = GDT.unElement . toElement $ _element_raw x
  mdc <- jsg   ("mdc"          :: Text)
  tf  <- mdc ! ("textField"    :: Text)
  tf' <- tf  ! ("MDCTextField" :: Text)
  void $ new tf' x'
