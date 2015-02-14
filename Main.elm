module Palmyra where

import Interface
import System
import System.Stock (Stock(..))
import System.Flow (Flow)
import Timing

import Graphics.Element (Element)
import Maybe
import Mouse
import Signal ((<~), (~), constant, foldp, Signal)

port speed : Maybe Float
timeDilation = Maybe.withDefault 1.0 speed
plyPerSecond = 10

startState = 
  let
    stocks = [ Mass "start" 200, Cap "middle" 30 20, Mass "end" 0, Ground, Charge "Happiness" -1 ]
    flows = [(1, "start","middle"), (1, "middle", "end"), (2, "ground", "end")]
  in System.new stocks flows

main = 
  let t = Timing.time timeDilation
  in display <~ systemUpdate startState t ~ t

display : System.System -> Float -> Element
display = Interface.display << System.getInfo

systemUpdate : System.System -> Signal Float -> Signal System.System
systemUpdate = foldp (System.update << Timing.discrete plyPerSecond)
