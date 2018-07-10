{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson               (ToJSON)
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.List                (intercalate)
import qualified Data.Map                 as M
import           GHC.Generics             (Generic)
import           Miso
import           Miso.String              (fromMisoString, ms)
import qualified Miso.Svg                 as Svg

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
      stats     :: [Stat]
    , nextId    :: StatId
    , nextLabel :: StatLabel
    } deriving (Eq, Show)

type StatId = Int
type StatLabel = String

data Stat = Stat {
      ident :: StatId
    , label :: StatLabel
    , value :: Int
    } deriving (Eq, Show, Generic)

instance ToJSON Stat

initialModel :: Model
initialModel = Model {
      stats = [
          Stat 0 "A" 100
        , Stat 1 "B" 100
        , Stat 2 "C" 100
        , Stat 3 "D" 100
        , Stat 4 "E" 100
        , Stat 5 "F" 100
        ]
    , nextId = 6
    , nextLabel = ""
    }

data Action =
      NoOp
    | SetLabel StatLabel
    | SetValue StatId Int
    | AddStat
    | DeleteStat StatId

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp model =
    noEff model
updateModel (SetLabel lbl) model =
    noEff model { nextLabel = lbl }
updateModel (SetValue sid val) model@(Model ss _ _) =
    noEff model { stats = map (update sid val) ss }
    where
        update sid val s = if ident s == sid then s { value = val } else s
updateModel AddStat model@(Model ss nid nlbl) =
    if not (null nlbl)
        then noEff $ Model (ss ++ [Stat nid nlbl 100]) (nid + 1) ""
        else noEff model
updateModel (DeleteStat sid) model@(Model ss _ _) =
    noEff model { stats = filter (\s -> ident s /= sid) ss }

viewModel :: Model -> View Action
viewModel model = div_ [] [
      div_ [ id_ "demo" ] $
           [ graphField $ stats model ]
        ++ (map statSlider $ stats model)
        ++ [ addForm ]
        ++ [ rawField $ stats model ]
    , footnote
    ]

graphField :: [Stat] -> View Action
graphField ss =
    Svg.svg_ [ Svg.width_ "200", Svg.height_ "200" ] [ polygraph ss ]

polygraph :: [Stat] -> View Action
polygraph ss = Svg.g_ [] $ [
      Svg.polygon_ [ Svg.points_ . ms $ toPoints ss ] []
    , Svg.circle_ [
          Svg.cx_ "100"
        , Svg.cy_ "100"
        , Svg.r_ "80"
        ] []
    ] ++ toLables ss

toPoints :: [Stat] -> String
toPoints ss = unwords $ map toString $
    zipWith toPoint (map (fromIntegral . value) ss) (anglesOf (length ss))
    where
        toPoint v th =
            ( 100 + v * 80 * sin th / 100
            , 100 - v * 80 * cos th / 100
            )
        toString (x, y) = intercalate "," [ show x, show y ]

toLables :: [Stat] -> [View action]
toLables ss = zipWith toText (map label ss) $
    zipWith toPoint (map (fromIntegral . value) ss) (anglesOf (length ss))
    where
        toPoint v th =
            ( 100 + (v + 10) * 80 * sin th / 100
            , 100 - (v + 10) * 80 * cos th / 100
            )
        toText l (x, y) =
            Svg.text_
                [ Svg.x_ . ms $ show x, Svg.y_ . ms $ show y]
                [ text $ ms l ]

anglesOf :: (Floating a) => Int -> [a]
anglesOf m = map toAngle [0 .. m - 1]
    where
        toAngle n = 2 * fromIntegral n * pi / fromIntegral m

statSlider :: Stat -> View Action
statSlider stat = div_ [] [
      label_ [] [ text . ms $ label stat ]
    , input_ [
          type_ "range"
        , min_ "0"
        , max_ "100"
        , value_ . ms $ value stat
        , onInput $ SetValue (ident stat) . read . fromMisoString
        ]
    , span_ [] [ text . ms . show $ value stat ]
    , button_ [ onClick $ DeleteStat (ident stat) ] [ text "X" ]
    ]

addForm :: View Action
addForm = div_ [] [
      input_ [ onInput $ SetLabel . fromMisoString ]
    , button_ [ onClick AddStat ] [ text "Add a Stat" ]
    ]

rawField :: [Stat] -> View Action
rawField ss = pre_ [ id_ "raw" ] [ text . ms $ encodePretty ss ]

footnote :: View action
footnote = p_ [ style_ $ M.fromList [("font-size", "12px")] ]
    [ text  "* input[type=\"range\"] requires IE10 or above." ]
