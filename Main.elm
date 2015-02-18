module Palmyra where

import Interface
import System
import System.Stock (Stock(..))
import Timing

import Maybe
import Signal ((<~), (~), foldp)

--import Data.CompoundInterest (bankAccount)
---------------
import Dict

interestRate = 0.03

bankAccount = {
    stocks = Dict.fromList [(1, Mass "Account" 100), (2, Ground "Interest")],
    flows = [ interestFlow ]
  }

interestFlow = {
    source = 2, sink = 1
  }

-----------------

port speed : Maybe Float
timeDilation = Maybe.withDefault 1.0 speed
plyPerSecond = 10

startState = System.new bankAccount

main = 
  let
    t = Timing.time timeDilation
    display = Interface.display << System.getInfo
    systemUpdate = foldp (System.update << Timing.discrete plyPerSecond)
  in display <~ systemUpdate startState t ~ t
