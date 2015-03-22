module Palmyra where

import Maybe
import Signal (..)
import Dict

import AnimationFrame
import Time
import Mouse

import Interface

import System
import Data.Agricola as Agri

port speed : Maybe Float

timeDilation = (Maybe.withDefault 1.0 speed) / Agri.turnDuration
plyPerTurn = Agri.plyPerTurn
startState = Agri.start

main = 
  let
    display = Interface.display << System.getInfo
    update = foldp (always System.evolve) startState ply
  in display <~ update ~ time

ply : Signal Int
ply = dropRepeats <| floor << (*) plyPerTurn <~ time

time : Signal Float
time = Time.inSeconds <~ foldp tick 0 (AnimationFrame.frameWhen pause)

pause : Signal Bool
pause = foldp (always not) True Mouse.clicks

tick : Time.Time -> Time.Time -> Time.Time
tick dt t = t + dt * timeDilation
