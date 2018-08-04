{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Char       (isSpace)
import           Data.List       (intersperse)
import           Data.List.Split (splitOn)
import           Miso
import           Miso.String     (fromMisoString, ms)

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
        , input_ $ [
              type_ "submit"
            , value_ "Add User"
            ] ++ if valid then [ onClick AddUser ] else []
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

viewUser :: Int -> User -> View Action
viewUser n user = li_ [ class_ "user" ] [
      span_ [] [ text . ms $ name user ++ " - " ++ email user ++ " " ]
    , button_ [ onClick (RemoveUser n) ] [ text "X" ]
    ]

validName :: String -> Bool
validName = not . all isSpace

-- Todo: regex-posix library doesn't work in GHCJS
validEmail :: String -> Bool
validEmail email = length strs == 2 && all noSpace strs
    where
        strs = splitOn "@" email
        noSpace str = not (null str) && not (any isSpace str)
