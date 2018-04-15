{-# LANGUAGE OverloadedStrings #-}
module MDC.Button where

import           Data.Text (Text)
import           Data.Map (Map)
import           Data.Monoid ((<>))

import           Reflex.Dom

buttonDynAttr :: (PostBuild t m, DomBuilder t m)
              => Dynamic t (Map Text Text) -> m a -> m (Event t ())
buttonDynAttr attrs = fmap (domEvent Click . fst)
                   . elDynAttr' "button" (attrs' <$> attrs)
  where attrs' attrs = "class"              =: "mdc-button mdc-card__action mdc-card__action--button"
                    <> "data-mdc-auto-init" =: "MDCRipple"
                    <> attrs

buttonAttr :: (PostBuild t m, DomBuilder t m)
           => Map Text Text -> m a -> m (Event t ())
buttonAttr attrs = fmap (domEvent Click . fst)
                . elAttr' "button" attrs'
  where attrs' = "class"              =: "mdc-button mdc-card__action mdc-card__action--button"
              <> "data-mdc-auto-init" =: "MDCRipple"
              <> attrs

dynFABIcon :: (PostBuild t m, DomBuilder t m) => Dynamic t Text -> m (Event t ())
dynFABIcon = fmap (domEvent Click . fst)
        . elAttr' "button" attrs
        . elClass "span" "mdc-fab__icon"
        . dynText
  where attrs = "class"              =: "mdc-fab material-icons"
             <> "aria-label"         =: "switch"
             <> "data-mdc-auto-init" =: "MDCRipple"
