module Palmyra where

import Interface
import System
import System.Types (Stock(..))

import Signal ((<~), (~), constant, foldp, Signal)
import Maybe
import Mouse
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
    stocks = [("start", Charge 500), ("middle", Charge 20), ("end", Charge 7)]
    flows = [(2, "start","middle"), (1, "middle", "end")]
  in System.new stocks flows
