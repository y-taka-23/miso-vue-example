{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List   (intersperse)
import           Miso
import           Miso.String (fromMisoString, ms)

main :: IO ()
main = do
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
      users    :: [User]
    , newName  :: String
    , newEmail :: String
    } deriving (Eq, Show)

data User = User {
      name  :: String
    , email :: String
    } deriving (Eq, Show)

initialModel :: Model
initialModel = Model {
      users = [
          User "Alice" "alice@example.com"
        , User "Bob"   "bob@example.com"
        , User "Carol" "carol@example.com"
        ]
    , newName = ""
    , newEmail = ""
    }

data Action =
      NoOp
    | SetNewName String
    | SetNewEmail String
    | AddUser
    | RemoveUser Int

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp model = noEff model
updateModel (SetNewName name) model = noEff model { newName = name }
updateModel (SetNewEmail email) model = noEff model { newEmail = email }
updateModel AddUser model =
    let user = User (newName model) (newEmail model)
    in noEff model {
          users = users model ++ [user]
        , newName = ""
        , newEmail = ""
        }
updateModel (RemoveUser n) model =
    noEff model { users = removeAt n (users model) }

removeAt :: Int -> [a] -> [a]
removeAt _ [] = []
removeAt 0 (_ : xs) = xs
removeAt n (x : xs) = x : removeAt (n - 1) xs

viewModel :: Model -> View Action
viewModel model = div_ [ id_ "app" ] [
      ul_ [] $ zipWith viewUser [0..] (users model)
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
        , input_ [
              type_ "submit"
            , onClick AddUser
            , value_ "Add User"
            ]
        ]
    , ul_ [ class_ "errors" ] [
          li_ [] [ text "Name cannot be empty." ]
        , li_ [] [ text "Please provide a valid email address." ]
        ]
    ]

viewUser :: Int -> User -> View Action
viewUser n user = li_ [ class_ "user" ] [
      span_ [] [ text . ms $ name user ++ " - " ++ email user ++ " " ]
    , button_ [ onClick (RemoveUser n) ] [ text "X" ]
    ]
