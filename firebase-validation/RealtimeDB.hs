{-# LANGUAGE DeriveGeneric #-}

module RealtimeDB (
      initializeFirebase
    , getUsersRef
--    , pushUser
    , DBRef
    , User(..)
    , UserKey
    ) where

import           Control.Lens                ((^.))
import qualified Data.Text                   as T
import           GHC.Generics                (Generic)
import           GHCJS.Types                 (JSVal)
import           Language.Javascript.JSaddle (ToJSVal, js0, js1, jsg,
                                              runJSaddle, val)

data FirebaseConfig = FirebaseConfig {
      apiKey      :: String
    , authDomain  :: String
    , databaseURL :: String
    } deriving (Eq, Show, Generic)

instance ToJSVal FirebaseConfig

config :: FirebaseConfig
config = FirebaseConfig {
      apiKey      = "<API_KEY>"
    , authDomain  = "<AUTH_DOMAIN>"
    , databaseURL = "<DATABASE_URL>"
    }

initializeFirebase :: IO ()
initializeFirebase = runJSaddle () $ do
    _ <- jsg "firebase" ^. js1 "initializeApp" (val config)
    return ()

type UserKey = T.Text

data User = User {
      name  :: String
    , email :: String
    } deriving (Eq, Show, Generic)

type DBRef = JSVal

getUsersRef :: IO DBRef
getUsersRef = runJSaddle () $ do
    ref <- jsg "firebase" ^. js0 "database" ^. js1 "ref" (val "/users")
    return ref
