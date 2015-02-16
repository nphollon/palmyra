module Palmyra where

import Interface
import System
import System.Stock (Stock(..))
import Timing

import Maybe
import Signal ((<~), (~), foldp)

port speed : Maybe Float
timeDilation = Maybe.withDefault 1.0 speed
plyPerSecond = 10

startState = 
  let
    stocks = [ Mass "start" 200, Mass "middle" 20, Mass "end" 0, Ground "The Void" ]
    flows = [(1, "start","middle"), (1, "middle", "end"), (2, "The Void", "end")]
  in System.new stocks flows

main = 
  let
    t = Timing.time timeDilation
    display = Interface.display << System.getInfo
    systemUpdate = foldp (System.update << Timing.discrete plyPerSecond)
  in display <~ systemUpdate startState t ~ t
