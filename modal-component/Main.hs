{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Miso

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
      showModal :: Bool
    } deriving (Eq, Show)

initialModel :: Model
initialModel = Model {
      showModal = False
    }

data Action =
      NoOp
    | ShowModal
    | HideModal

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp model      = noEff model
updateModel ShowModal model = noEff model { showModal = True }
updateModel HideModal model = noEff model { showModal = False }

viewModel :: Model -> View Action
viewModel model = div_ [ id_ "app" ] $
    [ button_
        [ id_ "show-modal", onClick ShowModal ]
        [ text "Show Modal" ]
    ] ++
    (if showModal model then [ modal ] else [])

modal :: View Action
modal = div_ [ class_ "modal-mask" ] [
      div_ [ class_ "modal-wrapper" ] [
          div_ [ class_ "modal-container" ] [
              div_ [ class_ "modal-header" ] [
                  span_ [ id_ "header" ] [ h3_ [] [text "default header" ] ]
                ]
            , div_ [ class_ "modal-body" ] [
                  span_ [ id_ "body" ] [ text "default body" ]
                ]
            , div_ [ class_ "modal-footer" ] [
                  span_ [ id_ "footer" ] [
                      text "default footer"
                    , button_
                        [ class_ "modal-default-button", onClick HideModal ]
                        [ text "OK" ]
                    ]
                ]
            ]
        ]
    ]
