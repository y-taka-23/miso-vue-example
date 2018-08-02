{-# LANGUAGE OverloadedStrings #-}

module Main where

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

type Model = Item

data Item =
      File   String
    | Folder String Bool [Item]
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
    | Here  Operation
    | Where Int Action
    deriving (Eq, Show)

data Operation =
      Toggle
    | ChangeType
    deriving (Eq, Show)

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp model = noEff model
updateModel act  model = noEff $ updateItem act model

updateItem :: Action -> Item -> Item
updateItem (Here op) item     = operateItem op item
updateItem (Where n act) item = updateChild n act item

operateItem :: Operation -> Item -> Item
operateItem Toggle file@(File _) = file
operateItem Toggle (Folder name open children) = Folder name (not open) children
operateItem ChangeType (File name) = Folder name True [ File "new stuff" ]
operateItem ChangeType folder@(Folder _ _ _) = folder

updateChild :: Int -> Action -> Item -> Item
updateChild n act file@(File _) = file
updateChild n act (Folder name open children) =
    Folder name open $ modifyAt n (updateItem act) children

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt _ _ []       = []
modifyAt 0 f (x : xs) = f x : xs
modifyAt n f (x : xs) = x : modifyAt (n - 1) f xs

viewModel :: Model -> View Action
viewModel model = div_ [] [
      p_ [] [
          text "(You can double click on an item to turn it into a folder.)"
        ]
    , ul_ [ id_ "demo" ] [ viewItem Here model ]
    ]

viewItem :: (Operation -> Action) -> Item -> View Action
viewItem this (File name) =
    li_ [ class_ "item" ] [
          div_ [ onDoubleClick (this ChangeType) ] [ text . ms $ name ]
        ]
viewItem this (Folder name open children) =
    li_ [ class_ "item" ] $ [
          div_ [ class_ "bold", onClick (this Toggle) ] [
              text . ms $ name ++ " "
            , span_ [] [ text $ if open then "[-]" else "[+]" ]
            ]
        ] ++ if open then [ viewChildren ] else []
        where
            viewChildren = ul_ [] $
                zipWith (\n i -> viewItem (n `of_` this) i) [0..] children
                ++ [ li_ [ class_ "add" ] [ text "+" ] ]

of_ :: Int -> (Operation -> Action) -> (Operation -> Action)
of_ n this = insert n . this

insert :: Int -> Action -> Action
insert n (Here op)     = Where n (Here op)
insert n (Where m act) = Where m (insert n act)
