module Palmyra where

import Interface
import System

import Signal ((<~), (~), constant, foldp, Signal)
import Maybe
import Mouse
import Time

port speed : Maybe Float

main = Interface.display <~ system ~ (fst <~ time)

timeDilation = Maybe.withDefault 1.0 speed
plyPerSecond = 10

system = foldp System.update startState (snd <~ time)

time = foldp tick (0,0) (Time.fpsWhen 60 pause)

tick dt (t, ply) =
  let 
    tNext = Time.inSeconds dt * timeDilation + t
    plyNext = tNext * plyPerSecond |> floor
  in (tNext, plyNext)

pause = foldp (always not) False Mouse.clicks

startState = 
  let
    stocks = [("start", 500), ("middle", 20), ("end", 7)]
    flows = [("start","middle"), ("middle", "end")]
  in System.new stocks flows
