{-# LANGUAGE OverloadedStrings #-}

module Main where

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

type Model = Item

data Item =
      File   String
    | Folder String Bool [Item]
    deriving (Eq, Show)

initialModel :: Model
initialModel =
    Folder "My Tree" False [
          File "hello"
        , File "wat"
        , Folder "child folder" False [
              Folder "child folder" False [
                  File "hello"
                , File "wat"
                ]
            , File "hello"
            , File "wat"
            , Folder "child folder" False [
                  File "hello"
                , File "wat"
                ]
            ]
        ]

data Action =
      NoOp

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp model =
    noEff model

viewModel :: Model -> View Action
viewModel model = div_ [] [
      p_ [] [
          text "(You can double click on an item to turn it into a folder.)"
        ]
    , ul_ [ id_ "demo" ] [ viewItem model ]
    ]

viewItem :: Item -> View Action
viewItem (File name) =
    li_ [ class_ "item" ] [
          div_ [] [
              text . ms $ name
            ]
        ]
viewItem (Folder name open children) =
    li_ [ class_ "item" ] [
          div_ [ class_ "bold" ] [
              text . ms $ name
            , span_ [] [ text $ if open then "-" else "+" ]
            ]
        , ul_ [] $
            map viewItem children ++
            [ li_ [ class_ "add" ] [ text "+" ] ]
        ]
