module Palmyra where

import Maybe
import Signal (..)
import Dict

import AnimationFrame
import Time
import Mouse

import Interface

import System
import Data.Simple (smallSystems)

port speed : Maybe Float

timeDilation = Maybe.withDefault 1.0 speed
plyPerSecond = 10

main = 
  let
    display = Interface.display << System.getInfo
    update = foldp (always System.evolve) startState ply
  in display <~ update ~ time

ply : Signal Int
ply = dropRepeats <| floor << (*) plyPerSecond <~ time

time : Signal Float
time = foldp (tick timeDilation) 0 (AnimationFrame.frameWhen pause)

pause : Signal Bool
pause = foldp (always not) True Mouse.clicks

tick : Float -> Time.Time -> Float -> Float
tick dilation dt t = Time.inSeconds dt * dilation + t

startState = smallSystems
