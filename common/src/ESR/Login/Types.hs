{-# LANGUAGE CPP           #-}
{-# LANGUAGE DeriveGeneric #-}
module ESR.Login.Types where

import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
#if !__GHCJS__
import           Servant.Auth.Server    (FromJWT, ToJWT)
#endif

data Login = Login { username :: Text , password :: Text }
  deriving (Eq, Show, Read, Generic)

instance ToJSON   Login
instance FromJSON Login

data User = User { name :: Text, email :: Text }
  deriving (Eq, Show, Read, Generic)

instance ToJSON   User
instance FromJSON User

#if !__GHCJS__
instance ToJWT   User
instance FromJWT User
#endif
