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
      | Here  Operation
      | Where Int Action

data Operation =
      Toggle

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp model =
    noEff model

viewModel :: Model -> View Action
viewModel model = div_ [] [
      p_ [] [
          text "(You can double click on an item to turn it into a folder.)"
        ]
    , ul_ [ id_ "demo" ] [ viewItem Here model ]
    ]

viewItem :: (Operation -> Action) -> Item -> View Action
viewItem _ (File name) =
    li_ [ class_ "item" ] [ div_ [] [ text . ms $ name ] ]
viewItem this (Folder name open children) =
    li_ [ class_ "item" ] [
          div_ [ class_ "bold", onClick (this Toggle) ] [
              text . ms $ name ++ " "
            , span_ [] [ text $ if open then "[-]" else "[+]" ]
            ]
        , ul_ [] $
            zipWith (\n i -> viewItem (n `of_` this) i) [0..] children ++
            [ li_ [ class_ "add" ] [ text "+" ] ]
        ]

of_ :: Int -> (Operation -> Action) -> (Operation -> Action)
of_ n this = Where n . this
