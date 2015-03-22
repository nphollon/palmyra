module Data.Agricola (start, turnDuration, plyPerTurn) where

import System (..)
import Dict

turnDuration = 24.0
plyPerTurn = 360

start = Systemic {
  stocks = Dict.fromList [
    ("Population", 1000),
    ("Crops", 0),
    ("Stockpile", 150),
    ("Hunger", 0),
    ("Time", 0) ],
  flows = Dict.fromList [
    ("Counting", Constant "Time" (1.0/30)),
    ("Growing", Constant "Crops" 0),
    ("Harvesting", Transfer "Crops" "Stockpile" 0.5) ],
  rules = Dict.fromList [
    ("Seasonal", { target = "Growing", rule = (transform1 "Time" seasonal) } ) ]
  }

seasonal t =
  let r = floor t % 12
  in if | r <= 2 -> 0
        | r >= 7 -> 1
        | otherwise -> -5
