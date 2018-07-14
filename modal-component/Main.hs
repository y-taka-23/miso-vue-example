{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Miso
import           Miso.String (MisoString)

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
updateModel NoOp model = noEff model
updateModel ShowModal model = noEff model { showModal = True }
updateModel HideModal model = noEff model { showModal = False }

viewModel :: Model -> View Action
viewModel model = div_ [ id_ "app" ] $
    [ button_
        [ id_ "show-modal", onClick ShowModal ]
        [ text "Show Modal" ]
    ] ++
   (if showModal model then [ attachShadow modalTemplate modal ] else [])

modalTemplate :: View Action
modalTemplate = div_ [ class_ "modal-mask" ] [
      div_ [ class_ "modal-wrapper" ] [
          div_ [ class_ "modal-container" ] [
              div_ [ class_ "modal-header" ] [
                  slot [ name_ "header" ] [ modal ]
                ]
            , div_ [ class_ "modal-body" ] [
                  slot [ name_ "body" ] [ text "default body" ]
                ]
            , div_ [ class_ "modal-footer" ] [
                  slot [ name_ "footer" ] [
                      text "default footer"
                    , button_
                        [ class_ "modal-default-button", onClick HideModal ]
                        [ text "OK" ]
                    ]
                ]
            ]
        ]
    ]

modal :: View Action
modal = div_ [] [
      h3_ [ slot_ "header" ] [ text "custom header" ]
    ]

-- Todo: not implemented yet: shadowRoot.innerHTML
attachShadow :: View action -> View action -> View action
attachShadow shadow host = shadow

slot :: [Attribute action] -> [View action] -> View action
slot = nodeHtml "slot"

slot_ :: MisoString -> Attribute action
slot_ = textProp "slot"
