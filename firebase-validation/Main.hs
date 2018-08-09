{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Char       (isSpace)
import           Data.List       (intersperse)
import           Data.List.Split (splitOn)
import           Miso
import           Miso.String     (fromMisoString, ms)
import           RealtimeDB      (User (..), UserKey, initializeFirebase,
                                  pushUser, removeUser)

main :: IO ()
main = do
    initializeFirebase
    startApp App {
      initialAction = NoOp
    , model = initialModel
    , update = updateModel
    , view = viewModel
    , subs = []
    , events = defaultEvents
    , mountPoint = Nothing
    }

data Model = Model {
      users    :: [(UserKey, User)]
    , newName  :: String
    , newEmail :: String
    } deriving (Eq, Show)

initialModel :: Model
initialModel = Model {
      users = []
    , newName = ""
    , newEmail = ""
    }

data Action =
      NoOp
    | SetNewName String
    | SetNewEmail String
    | PushUser
    | RemoveUser UserKey

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp model = noEff model
updateModel (SetNewName name) model = noEff model { newName = name }
updateModel (SetNewEmail email) model = noEff model { newEmail = email }
updateModel PushUser model = newModel <# do
    key <- pushUser user
    putStrLn $ "Pushed: " ++ show key
    pure NoOp
    where
        newModel = model { newName = "", newEmail = "" }
        user = User (newName model) (newEmail model)
updateModel (RemoveUser key) model = model <# do
    removeUser key
    putStrLn $ "Removed: " ++ show key
    pure NoOp

viewModel :: Model -> View Action
viewModel model = div_ [ id_ "app" ] [
      ul_ [] $ map viewUser (users model)
    , div_ [ id_ "form" ] $ intersperse (text " ") [
          input_ [
              type_ "text"
            , onInput (SetNewName . fromMisoString)
            , value_ . ms . newName $ model
            , placeholder_ "Username"
            ]
        , input_ [
              type_ "email"
            , onInput (SetNewEmail . fromMisoString)
            , value_ . ms . newEmail $ model
            , placeholder_ "email@email.com"
            ]
        , input_ $ [
              type_ "submit"
            , value_ "Add User"
            ] ++ if valid then [ onClick PushUser ] else []
        ]
    , ul_ [ class_ "errors" ] $ nameError ++ emailError
    ]
    where
        valid = nameValid && emailValid
        nameValid = validName $ newName model
        emailValid = validEmail $ newEmail model
        nameError = if not nameValid
            then [ li_ [] [ text "Name cannot be empty." ] ]
            else []
        emailError = if not emailValid
            then [ li_ [] [ text "Please provide a valid email address." ] ]
            else []

viewUser :: (UserKey, User) -> View Action
viewUser (key, user) = li_ [ class_ "user" ] [
      span_ [] [ text . ms $ name user ++ " - " ++ email user ++ " " ]
    , button_ [ onClick (RemoveUser key) ] [ text "X" ]
    ]

validName :: String -> Bool
validName = not . all isSpace

-- Todo: regex-posix library doesn't work in GHCJS
validEmail :: String -> Bool
validEmail email = length strs == 2 && all noSpace strs
    where
        strs = splitOn "@" email
        noSpace str = not (null str) && not (any isSpace str)
