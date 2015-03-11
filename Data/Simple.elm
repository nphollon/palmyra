module Data.Simple (smallSystems) where

import System (SystemParams)
import System.Stock (Stock(..))
import System.Flow (Flow(..))
import Dict

smallSystems : SystemParams
smallSystems = {
    stocks = Dict.fromList [ ("Room", Mass 285), ("Account", Mass 100) ],
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