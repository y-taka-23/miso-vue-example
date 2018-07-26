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

type Model = Object

data Object =
      File   String
    | Folder String Bool [Object]
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
viewModel model = div_ [] [ text "Tree View Example"]
