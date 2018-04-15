{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE OverloadedStrings #-}

import           Reflex.Dom
import           Reflex.Network

import           ESR.Login                         as Login

main :: IO ()
main = mainWidgetInElementById "esr" $ mdo
  model <- Login.makeModel conf
  confEvent <- networkView $ pure (Login.widget model)
  conf <- Login.Config <$> switchHold never (Login.newXSRF <$> confEvent)
  blank
