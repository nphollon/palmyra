module Palmyra where

import Interface
import System
import Timing

import Maybe
import Signal ((<~), (~), foldp)
import Dict

import Data.Simple (smallSystems)

port speed : Maybe Float
timeDilation = Maybe.withDefault 1.0 speed
plyPerSecond = 10

startState = System.new smallSystems

main = 
  let
    t = Timing.time timeDilation
    display = System.getInfo >> Interface.display
    systemUpdate = foldp (Timing.discrete plyPerSecond >> System.update)
  in display <~ systemUpdate startState t ~ t
