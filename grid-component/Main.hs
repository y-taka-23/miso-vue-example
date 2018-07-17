{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Char   (toLower, toUpper)
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

data Model = Model {
      gridData :: [Fighter]
    } deriving (Eq, Show)

data Fighter = Fighter {
      fightName  :: String
    , fightPower :: Power
    } deriving (Eq, Show)

data Power =
      Power Int
    | Infinity
    deriving (Eq)

instance Show Power where
    show (Power n) = show n
    show Infinity  = "Infinity"

initialModel :: Model
initialModel = Model {
      gridData = [
          Fighter "Chuck Norris" Infinity
        , Fighter "Bruce Lee"    (Power 9000)
        , Fighter "Jackie Chan"  (Power 7000)
        , Fighter "Jet Li"       (Power 8000)
        ]
    }

data Action =
      NoOp

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp model =
    noEff model

viewModel :: Model -> View action
viewModel model = div_ [ id_ "demo" ] [
      form_ [ id_ "search" ] [
          text "Search "
        , input_ [ name_ "query" ]
        ]
    , gridTable (gridData model)
    ]

gridTable :: [Fighter] -> View action
gridTable fs = table_ [] [
      thead_ [] [ gridHeader ]
    , tbody_ [] $ map gridRecord fs
    ]

gridHeader :: View action
gridHeader = tr_ [] [
      th_ [ class_ "active" ] [
          text . ms $ capitalize "name" ++ " "
        , span_ [ classList_ [("arrow", True), ("asc", True)] ] []
        ]
    , th_ [] [
          text . ms $ capitalize "power" ++ " "
        , span_ [ classList_ [("arrow", True), ("dsc", True)] ] []
        ]
    ]

capitalize :: String -> String
capitalize []       = []
capitalize (c : cs) = toUpper c : map toLower cs

gridRecord :: Fighter -> View action
gridRecord f = tr_ [] [
      td_ [] [ text . ms $ fightName f ]
    , td_ [] [ text . ms . show $ fightPower f ]
    ]
