import Interface
import System

import Signal ((<~), foldp, Signal)
import Mouse
import Time


main = Interface.display <~ systemSignal

systemSignal = foldp System.evolve startState intervals

startState = 
  let
    stocks = [("start", 500), ("middle", 20), ("end", 7)]
    flows = [("start","middle"), ("middle", "end")]
  in System.new stocks flows

intervals = Time.inSeconds <~ Time.fpsWhen 60 pause

pause = foldp (\_ b -> not b) False Mouse.clicks
