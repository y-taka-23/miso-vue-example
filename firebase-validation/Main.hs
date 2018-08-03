{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List   (intersperse)
import           Miso
import           Miso.String (ms)

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
      users   :: [User]
    , newUser :: User
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
    , newUser = User "" ""
    }

data Action =
      NoOp

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp model = noEff model

viewModel :: Model -> View Action
viewModel model = div_ [ id_ "app" ] [
      ul_ [] $ map viewUser (users model)
    , div_ [ id_ "form" ] $ intersperse (text " ") [
          input_ [
              type_ "text"
            , value_ . ms . name . newUser $ model
            , placeholder_ "Username"
            ]
        , input_ [
              type_ "email"
            , value_ . ms . email . newUser $ model
            , placeholder_ "email@email.com"
            ]
        , input_ [ type_ "submit", value_ "Add User" ]
        ]
    , ul_ [ class_ "errors" ] [
          li_ [] [ text "Name cannot be empty." ]
        , li_ [] [ text "Please provide a valid email address." ]
        ]
    ]

viewUser :: User -> View Action
viewUser user = li_ [ class_ "user" ] [
      span_ [] [ text . ms $ name user ++ " - " ++ email user ++ " " ]
    , button_ [] [ text "X" ]
    ]
