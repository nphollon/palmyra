module Data.Simple (smallSystems) where

import System (SystemParams)
import System.Flow (Flow(..))
import Dict

smallSystems : SystemParams
smallSystems = {
    stocks = Dict.fromList [ ("Room", 285), ("Account", 100) ],
    flows = Dict.fromList [ ("Heating", Constant "Room" 0), ("Cooling", Decay "Room" 0.001 265), ("Interest", Growth "Account" 0.0003) ],
    rules = Dict.fromList [ ("Thermostat", heatTrigger) ]
  }

heatTrigger = {
  target = "Heating",
  dependsOn = "Room",
  rule t =
    if | t < 287 -> Just 0.1
       | t > 290 -> Just 0
       | otherwise -> Nothing
  }