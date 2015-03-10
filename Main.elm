module Palmyra where

import Interface
import System
import Timing

import Maybe
import Signal ((<~), (~), foldp)
import Dict

import Data.CompoundInterest (bankAccount)
import Data.Thermostat (thermostat)

port speed : Maybe Float
timeDilation = Maybe.withDefault 1.0 speed
plyPerSecond = 10

startState = System.new (merge bankAccount thermostat)

main = 
  let
    t = Timing.time timeDilation
    display = System.getInfo >> Interface.display
    systemUpdate = foldp (Timing.discrete plyPerSecond >> System.update)
  in display <~ systemUpdate startState t ~ t

merge : System.SystemParams -> System.SystemParams -> System.SystemParams
merge s1 s2 = {
    stocks = Dict.union s1.stocks s2.stocks,
    flows = s1.flows ++ s2.flows
  }