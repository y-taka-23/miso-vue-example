{-# LANGUAGE DeriveGeneric     #-}
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

data Model = Model
     deriving (Eq, Show)

initialModel :: Model
initialModel = Model

data Action =
      NoOp

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp model =
    noEff model

viewModel :: Model -> View Action
viewModel model = div_ [] [ text "Tree View Example"]
