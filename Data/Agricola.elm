module Data.Agricola (start, turnDuration, plyPerTurn) where

import System (..)
import Dict

turnDuration = 24
plyPerTurn = 360

start = Systemic {
  stocks = Dict.fromList [
    ("Population", 100),
    ("Crops", 0),
    ("Stockpile", 500),
    ("Time", 0) ],
  flows = Dict.fromList [
    ("Counting", Constant "Time" (1.0/30)),
    ("Growing", Constant "Crops" 0),
    ("Harvesting", Transfer "Crops" "Stockpile" 0.5),
    ("Dying", Decay "Population" 4.186E-4 0),
    ("Birthing", Growth "Population" 6E-4),
    ("Eating", Constant "Stockpile" -100),
    ("Starving", Constant "Population" 0) ],
  rules = Dict.fromList [
    ("Seasons Rule", { target = "Growing", rule = (transform1 "Time" seasonal) } ),
    ("Labor Rule", { target = "Harvesting", rule = (transform1 "Population" laborious) } ),
    ("Eating Rule", { target = "Eating", rule = (transform1 "Population" eating) } ),
    ("Starving Rule", { target = "Dying", rule = (transform2 "Population" "Stockpile" starving) } ) ]
  }

seasonal t =
  let r = floor t % 12
  in if | r <= 2 -> 0
        | r >= 6 -> 6
        | otherwise -> -500

laborious p = p

eating p = negate (p / 30)

starving p s =
  let deathRate = 4.186E-4
  in if s * 30 < p then 5 * deathRate else deathRate