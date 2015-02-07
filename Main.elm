module Palmyra where

import Interface
import System
import System.Stock (Stock(..))

import Maybe
import Mouse
import Signal ((<~), (~), constant, foldp, Signal)
import Time

port speed : Maybe Float

main = Interface.display <~ (System.getInfo <~ system) ~ (fst <~ time)

timeDilation = Maybe.withDefault 1.0 speed
plyPerSecond = 10

system = foldp System.update startState (snd <~ time)

time = foldp tick (0,0) (Time.fpsWhen 60 pause)

tick dt (t, ply) =
  let 
    tNext = Time.inSeconds dt * timeDilation + t
    plyNext = tNext * plyPerSecond |> floor
  in (tNext, plyNext)

pause = foldp (always not) True Mouse.clicks

startState = 
  let
    stocks = [("start", Mass 200), ("middle", Mass 20), ("end", Mass 0), ("ground", Ground), ("Happiness", Charge -1)]
    flows = [(1, "start","middle"), (1, "middle", "end"), (2, "ground", "end")]
  in System.new stocks flows
