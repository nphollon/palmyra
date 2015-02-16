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
    stocks = [
      (1, Mass "start" 200),
      (2, Mass "middle" 20),
      (3, Mass "end" 0),
      (4, Ground "The Void")]
    flows = [
      (1.0, 1, 2),
      (1.0, 2, 3), 
      (2.0, 4, 3)]
  in System.new stocks flows

main = 
  let
    t = Timing.time timeDilation
    display = Interface.display << System.getInfo
    systemUpdate = foldp (System.update << Timing.discrete plyPerSecond)
  in display <~ systemUpdate startState t ~ t
