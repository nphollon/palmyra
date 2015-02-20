module Palmyra where

import Interface
import System
import Timing

import Maybe
import Signal ((<~), (~), foldp)

import Data.CompoundInterest (bankAccount)

port speed : Maybe Float
timeDilation = Maybe.withDefault 1.0 speed
plyPerSecond = 10

startState = System.new bankAccount

main = 
  let
    t = Timing.time timeDilation
    display = System.getInfo >> Interface.display
    systemUpdate = foldp (Timing.discrete plyPerSecond >> System.update)
  in display <~ systemUpdate startState t ~ t
