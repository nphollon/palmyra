module Data.Thermostat (thermostat) where

import System (SystemParams)
import System.Stock (Stock(..))
import Dict

opacity = 1000
outsideTemp = 265
targetTemp = 290
triggerTemp = 287
heat = 0.1

thermostat : SystemParams
thermostat = {
    stocks = Dict.fromList [ ("Furnace", Ground), ("Room", Mass 285), ("Outside", Ground) ],
    flows = [ heatRoom, coolRoom ]
  }

heatRoom = { source="Furnace", sink="Room", stateId="0", states=heatStates }
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

coolRoom = { source="Room", sink="Outside", stateId="0", states=coolStates }
coolStates = Dict.singleton "0" cooling
cooling =
  let flux i _ =
    case i of
      Just roomTemp -> (roomTemp - outsideTemp) / opacity
      Nothing -> 0
  in { flux = flux, rules = [] }
