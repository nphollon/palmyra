module Data.Thermostat (thermostat) where

import System (SystemParams)
import System.Stock (Stock(..))
import System.Flow (Flow(..))
import Dict

targetTemp = 290
triggerTemp = 287
heat = 0.1

thermostat : SystemParams
thermostat = {
    stocks = Dict.fromList [ ("Furnace", Ground), ("Room", Mass 285) ],
    flows = [ heatRoom, Decay "Room" 0.001 265 ]
  }

heatRoom = Deprecate { source="Furnace", sink="Room", rate=0, stateId="0", states=heatStates }
heatStates = Dict.fromList [ ("0", heatOn), ("1", heatOff) ]

heatOn =
  let
    flux _ _ = heat
    warmEnough _ o =
      case o of
        Just roomTemp -> roomTemp > targetTemp
        Nothing -> False
  in { flux = flux, rules = [ { trigger=warmEnough, newStateId="1" } ] }

heatOff =
  let
    flux _ _ = 0
    tooCold _ o =
      case o of
        Just roomTemp -> roomTemp < triggerTemp
        Nothing -> False
  in { flux = flux, rules = [ { trigger=tooCold, newStateId="0" } ] }
