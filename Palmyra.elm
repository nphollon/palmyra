import Interface
import System

import Signal ((<~), (~), constant, foldp, Signal)
import Mouse
import Time

main = Interface.display <~ system ~ (fst <~ time)

timeDilation = 1.0
plyPerSecond = 3

system = foldp System.update startState (snd <~ time)

time = foldp dilate (0,0) (Time.fpsWhen 60 pause)

dilate dt (t, ply) =
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
