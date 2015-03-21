module Data.Simple (smallSystems) where

import System (SystemParams, Flow(..), transform2)
import Dict

smallSystems : SystemParams
smallSystems = {
    stocks = Dict.fromList [ ("Room", 285), ("Account", 100) ],
    flows = Dict.fromList [ ("Heating", Constant "Room" 0), ("Cooling", Decay "Room" 0.001 265), ("Interest", Growth "Account" 0.0003) ],
    rules = Dict.fromList [ ("Thermostat", heatTrigger) ]
  }

heatTrigger = {
  target = "Heating",
  rule = transform2 "Room" "Heating" heatRule
  }

heatRule t h =
  if | t < 287 -> 0.1
     | t > 290 -> 0
     | otherwise -> h