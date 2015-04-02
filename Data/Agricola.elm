module Data.Agricola (start, turnDuration, plyPerTurn) where

import System (..)
import Dict

turnDuration = 24
plyPerTurn = 360

deathRate = 4.186E-4
birthRate = 6E-4
labor = 0.5
hunger = 30
day = 1.0/30

start = {
  stocks = [
    ("Population", 100),
    ("Crops", 0),
    ("Stockpile", 500),
    ("Time", 0) ],
  rates = [
    ("Day", day, always day),
    ("Growth", 0, transform1 "Time" seasonal),
    ("Death Rate", deathRate, transform2 "Population" "Stockpile" starving),
    ("Birth Rate", birthRate, always birthRate),
    ("Eating", -3, transform1 "Population" eating),
    ("Labor", 0.5, transform1 "Population" laborious)
  ],
  flows = [
    Flux add "Day" "Time",
    Flux add "Growth" "Crops",
    Flux decay "Death Rate" "Population",
    Flux grow "Birth Rate" "Population",
    Flux add "Eating" "Stockpile",
    Flux add "Starving" "Population",
    Transfer transfer "Labor" "Crops" "Stockpile" ]
  }

add dx x = max 0 (x + dx)

decay r p = p * (1 - r)

grow r p = p * (1 + r)

transfer r i o =
  if | i < r        -> (0, o + i)
     | o < negate r -> (i + o, 0)
     | otherwise    -> (i - r, o + r)

seasonal t =
  let r = floor t % 12
  in if | r <= 2 -> 0
        | r >= 6 -> 6
        | otherwise -> -500

laborious p = labor * p

eating p = negate (p / hunger)

starving p s = if s * 30 < p then 20 * deathRate else deathRate