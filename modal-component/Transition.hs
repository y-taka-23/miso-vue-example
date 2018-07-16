{-# LANGUAGE OverloadedStrings #-}

module Transition (
      State
    , visible
    , invisible
    , Trigger
    , enter
    , leave
    , trigger
    , transition_
    ) where

import qualified Data.Map    as M
import           Data.Monoid ((<>))
import           Miso
import           Miso.String (MisoString)

data State = State {
      stVisible   :: Bool
    , stTriggered :: Bool
    } deriving (Eq, Show)

visible :: State
visible = State {
      stVisible   = True
    , stTriggered = False
    }

invisible :: State
invisible = State {
      stVisible   = False
    , stTriggered = False
    }

data Trigger =
      Enter
    | Entering
    | Leave
    | Leaving

enter :: Trigger
enter = Enter

leave :: Trigger
leave = Leave

trigger :: Trigger -> State -> Effect Trigger State
trigger Enter st =
    st { stVisible = True, stTriggered = True } <# pure Entering
trigger Entering st =
    noEff st { stVisible = True, stTriggered = False }
trigger Leave st =
    st { stVisible = False, stTriggered = True } <# pure Leaving
trigger Leaving st =
    noEff st { stVisible = False, stTriggered = False }

transition_ :: MisoString -> State -> [View action] -> View action
transition_ name st components =
    div_ [ classes, visibility ] components
    where
        classes = classList_ [
              (name <> "-enter-active",      stVisible st)
            , (name <> "-enter",             stVisible st  && stTriggered st)
            , (name <> "-leave-active", not (stVisible st))
            , (name <> "-leave",        not (stVisible st) && stTriggered st)
            ]
        visibility = style_ $ if stVisible st
            then M.empty
            else M.singleton "visibility" "hidden"
