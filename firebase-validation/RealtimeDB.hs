{-# LANGUAGE DeriveGeneric #-}

module RealtimeDB (
      initializeFirebase
    , pushUser
    , DBRef
    , User(..)
    , UserKey
    ) where

import           Control.Lens                ((^.))
import qualified Data.Text                   as T
import           GHC.Generics                (Generic)
import           GHCJS.Types                 (JSVal)
import           Language.Javascript.JSaddle (ToJSVal, js, js0, js1, jsg,
                                              runJSaddle, val, valToText)

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

instance ToJSVal User

type DBRef = JSVal

getDBRef :: String -> JSM DBRef
getDBRef path = do
    ref <- jsg "firebase" ^. js0 "database" ^. js1 "ref" (val path)
    return ref

pushUser :: User -> IO UserKey
pushUser user = runJSaddle () $ do
    ref <- getDBRef "/users"
    res <- ref ^. js1 "push" (val user)
    key <- valToText =<< res ^. js "key"
    return key
