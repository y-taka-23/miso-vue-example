{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Char     (toLower, toUpper)
import           Data.Function (on)
import           Data.List     (isInfixOf, sortBy)
import qualified Data.Map      as M
import           Miso          hiding (on)
import           Miso.String   (fromMisoString, ms)

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
      sortKey   :: Maybe SortKey
    , sortOrder :: OrderSet
    , query     :: String
    , gridData  :: [Fighter]
    } deriving (Eq, Show)

data SortKey =
      FightName
    | FightPower
    deriving (Eq, Ord)

instance Show SortKey where
    show FightName  = "Name"
    show FightPower = "Power"

data SortOrder =
      Asc
    | Dsc
    deriving (Eq, Show)

type OrderSet = M.Map SortKey SortOrder

data Fighter = Fighter {
      fightName  :: String
    , fightPower :: Power
    } deriving (Eq, Show)

data Power =
      Power Int
    | Infinity
    deriving (Eq, Ord)

instance Show Power where
    show (Power n) = show n
    show Infinity  = "Infinity"

initialModel :: Model
initialModel = Model {
      sortKey = Nothing
    , sortOrder = M.fromList [(FightName, Asc), (FightPower, Asc)]
    , query = ""
    , gridData = [
          Fighter "Chuck Norris" Infinity
        , Fighter "Bruce Lee"    (Power 9000)
        , Fighter "Jackie Chan"  (Power 7000)
        , Fighter "Jet Li"       (Power 8000)
        ]
    }

data Action =
      NoOp
    | SortOn SortKey
    | Query String

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp model =
    noEff model
updateModel (SortOn key) model =
    noEff model {
          sortKey = Just key
        , sortOrder = M.adjust flipOrder key (sortOrder model)
        }
updateModel (Query q) model =
    noEff model { query = q }

flipOrder :: SortOrder -> SortOrder
flipOrder Asc = Dsc
flipOrder Dsc = Asc

viewModel :: Model -> View Action
viewModel model = div_ [ id_ "demo" ] [
      text "Search "
    , input_ [
          name_ "query"
        , type_ "text"
        , value_ $ ms (query model)
        , onInput $ Query . fromMisoString
        ]
    , gridTable mActiveKey orders fs
    ]
    where
        mActiveKey = sortKey model
        orders = sortOrder model
        fs = sortFighter mActiveKey orders $
            filterFighter (query model) (gridData model)

gridTable :: Maybe SortKey -> OrderSet -> [Fighter] -> View Action
gridTable mActiveKey orders fs = table_ [] [
      thead_ [] [ gridHeaders mActiveKey orders ]
    , tbody_ [] $ map gridRecord fs
    ]

gridHeaders :: Maybe SortKey -> OrderSet -> View Action
gridHeaders mActiveKey orders =
    tr_ [] $ map (gridHeader mActiveKey) $ M.toAscList orders

gridHeader :: Maybe SortKey -> (SortKey, SortOrder) -> View Action
gridHeader mActiveKey (key, order) =
    th_ [
          classList_ [("active", mActiveKey == Just key)]
        , onClick $ SortOn key
        ] [
          text . ms $ show key ++ " "
        , arrow order
        ]

arrow :: SortOrder -> View action
arrow Asc = span_ [ classList_ [("arrow", True), ("asc", True)] ] []
arrow Dsc = span_ [ classList_ [("arrow", True), ("dsc", True)] ] []

filterFighter :: String -> [Fighter] -> [Fighter]
filterFighter q = filter (querying q)
    where
        querying q f =
            any (\str -> q `isInfixOf` str) [fightName f, show (fightPower f)]

sortFighter mActiveKey orders =
    let condition = do
            activeKey <- mActiveKey
            order <- M.lookup activeKey orders
            return (activeKey, order)
    in case condition of
        Nothing                -> id
        Just (FightName,  Asc) -> sortBy        (compare `on` fightName)
        Just (FightName,  Dsc) -> sortBy $ flip (compare `on` fightName)
        Just (FightPower, Asc) -> sortBy        (compare `on` fightPower)
        Just (FightPower, Dsc) -> sortBy $ flip (compare `on` fightPower)

gridRecord :: Fighter -> View action
gridRecord f = tr_ [] [
      td_ [] [ text . ms $ fightName f ]
    , td_ [] [ text . ms . show $ fightPower f ]
    ]
