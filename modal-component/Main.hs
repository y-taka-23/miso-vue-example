{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Bifunctor (bimap)
import           Miso

import           Transition

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
      modalState :: State
    } deriving (Eq, Show)

initialModel :: Model
initialModel = Model {
      modalState = invisible
    }

data Action =
      NoOp
    | Modal Trigger

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp model =
    noEff model
updateModel (Modal trg) model =
    bimap Modal Model $ trigger trg (modalState model)

viewModel :: Model -> View Action
viewModel model = div_ [ id_ "app" ] $
    [ button_
        [ id_ "show-modal", onClick $ Modal enter ]
        [ text "Show Modal" ]
    , transition_ "modal" (modalState model) [ modal ]
    ]

modal :: View Action
modal = div_ [ class_ "modal-mask" ] [
      div_ [ class_ "modal-wrapper" ] [
          div_ [ class_ "modal-container" ] [
              div_ [ class_ "modal-header" ] [
                  span_ [ id_ "header" ] [ h3_ [] [ text "default header" ] ]
                ]
            , div_ [ class_ "modal-body" ] [
                  span_ [ id_ "body" ] [ text "default body" ]
                ]
            , div_ [ class_ "modal-footer" ] [
                  span_ [ id_ "footer" ] [
                      text "default footer"
                    , button_
                        [ class_ "modal-default-button", onClick $ Modal leave ]
                        [ text "OK" ]
                    ]
                ]
            ]
        ]
    ]
