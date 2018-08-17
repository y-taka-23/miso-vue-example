module Main where

import           Miso
import           Miso.String (ms)

main :: IO ()
main = startApp App {
      initialAction = NoOp
    , model = initialModel
    , update = updateModel
    , view = viewModel
    , subs = []
    , events = defaultEvents
    , mountPoint = Nothing
    }

type Model = String

initialModel :: Model
initialModel = "World"

data Action =
      NoOp

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp model = noEff model

viewModel :: Model -> View Action
viewModel model = div_ [] [ text . ms $ "Hello, " ++ model ++ "!" ]
