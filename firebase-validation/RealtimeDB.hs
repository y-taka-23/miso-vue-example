{-# LANGUAGE DeriveGeneric #-}

module RealtimeDB (
      initializeFirebase
    , pushUser
    , removeUser
    , usersSub
    , User(..)
    , UserKey
    ) where

import           Control.Lens                ((^.))
import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson                  (FromJSON, decode)
import           Data.JSString.Text          (lazyTextFromJSString)
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import           Data.Text.Lazy.Encoding     (encodeUtf8)
import           GHC.Generics                (Generic)
import           GHCJS.Types                 (JSVal)
import           Language.Javascript.JSaddle (JSM, ToJSVal, fun, js, js0, js1,
                                              js2, jsg, runJSaddle, val,
                                              valToJSON, valToText)
import           Miso                        (Sub)

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
instance FromJSON User

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

-- Todo: set callback in case of errors
removeUser :: UserKey -> IO ()
removeUser key = runJSaddle () $ do
    ref <- getDBRef $ "/users/" ++ T.unpack key
    _ <- ref ^. js0 "remove"
    return ()

usersSub :: (M.Map UserKey User -> action) -> Sub action
usersSub act = \sink -> do
    ref <- getDBRef "/users"
    _ <- ref ^. js2 "on" (val "value") (fun $ \_ _ [snapshot] -> do
        json <- valToJSON =<< snapshot ^. js0 "val"
        case (decode . encodeUtf8 . lazyTextFromJSString) json of
            Nothing -> liftIO (putStrLn $ "Parse failed: " ++ show json)
            Just us -> liftIO (sink $ act us))
    return ()
