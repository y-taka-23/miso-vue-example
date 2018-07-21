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

data Model = Model {
      controlPoint :: (Int, Int)
    } deriving (Eq, Show)

initialModel :: Model
initialModel = Model {
      controlPoint = (160, 160)
    }

data Action =
      NoOp

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp model = noEff model

viewModel :: Model -> View Action
viewModel model = div_ [ id_ "app" ] [
      div_ [
          class_ "draggable-header-view"
        ] [
          Svg.svg_ [
              class_ "bg"
            , Svg.width_ "320"
            , Svg.height_ "560"
            ] [
              Svg.path_ [
                  Svg.d_ . ms $ headerBezier (controlPoint model)
                , Svg.fill_ "#614e83"
                ] []
            ]
        , div_ [ class_ "header" ] [
              h1_ [] [ text "Elastic Draggable SVG Header" ]
            , subheader
            ]
        , div_ [ class_ "content" ] [ content ]
        ]
    ]

headerBezier :: (Int, Int) -> String
headerBezier (x, y) = unlines $ [
      "M 0,0"
    , "L 320,0"
    , "320, 160"
    , "Q " ++ show x ++ "," ++ show y
    , "0,160"
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
