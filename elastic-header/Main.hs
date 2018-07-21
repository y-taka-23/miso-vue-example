{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Miso
import           Miso.String (ms)
import qualified Miso.Svg    as Svg

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

data Model = Model
    deriving (Eq, Show)

initialModel :: Model
initialModel = Model

data Action =
      NoOp

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp model = noEff model

viewModel :: Model -> View Action
viewModel model = div_ [ id_ "app" ] [
      div_ [ class_ "draggable-header-view" ] [
          Svg.svg_ [ class_ "bg" ] []
        , div_ [ class_ "header" ] [
              h1_ [] [ text "Elastic Draggable SVG Header" ]
            , subheader
            ]
        , div_ [ class_ "content" ] [ content ]
        ]
    ]

subheader :: View action
subheader = p_ [] [
      text "with "
    , a_ [ href_ "https://haskell-miso.org" ] [ text "Miso" ]
    , text " + "
    , a_ [ href_ "http://dynamicsjs.com" ] [ text "dynamics.js" ]
    ]

content :: View action
content = p_ [] [
      text . ms . unlines $ [
          "Note this is just an effect demo -"
        , "there are of course many additional details"
        , "if you want to use this in production,"
        , "e.g. handling responsive sizes, reload threshold"
        , "and content scrolling."
        , "Those are out of scope for this quick little hack."
        , "However, the idea is that you can hide them"
        , "as internal details of the Miso framework"
        , "and expose its purely functional interface."
        ]
    ]
