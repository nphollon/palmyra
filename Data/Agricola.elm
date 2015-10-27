module Data.Agricola (start, display, turnDuration, plyPerTurn) where

import System (..)
import Array
import Maybe
import String

turnDuration = 24
plyPerTurn = 360

deathRate = 4.186E-4
birthRate = 6E-4
day = 1.0/30
hunger = day
labor = 0.067
arableLand = labor * 500

start = {
  stocks = [
    ("Population", 100),
    ("Stockpile", 500),
    ("Date", 0) ],
  rates = [
    ("Day", day, always day),
    ("Harvest", 0, transform2 "Population" "Date" seasonalGrowth),
    ("Death Rate", deathRate, transform2 "Population" "Stockpile" starving),
    ("Birth Rate", birthRate, always birthRate),
    ("Eating", -3, transform1 "Population" eating) ],
  flows = [
    Flux add "Day" "Date",
    Flux add "Harvest" "Stockpile",
    Flux decay "Death Rate" "Population",
    Flux grow "Birth Rate" "Population",
    Flux add "Eating" "Stockpile",
    Flux add "Starving" "Population" ]
  }

display = [
  ("Population", int),
  ("Date", date),
  ("Stockpile", int),
  ("Harvest", decimal)
  ]

add dx x = max 0 (x + dx)

decay r p = p * (1 - r)

grow r p = p * (1 + r)

transfer r i o =
  if | i < r        -> (0, o + i)
     | o < negate r -> (i + o, 0)
     | otherwise    -> (i - r, o + r)

seasonalGrowth p t =
  let r = floor t % 12
  in if | r >= 6 -> min arableLand (labor * p)
        | otherwise -> 0

eating p = negate (p * hunger)

starving p s =
  let a = if s < p * hunger then 20 else 1
  in a * deathRate

int = floor >> toString

decimal x =
  let
    totalCents = round (x * 100)
    dollars = totalCents // 100 |> toString
    cents = totalCents `rem` 100 |> abs |> toString |> String.pad 2 '0'
  in dollars ++ "." ++ cents

months = Array.fromList [ "September", "October", "November", "December", "January", "February", "March", "April", "May", "June", "July", "August" ]
month t =
  let m = floor t % 12
  in Array.get m months |> Maybe.withDefault "???"

year t = int (t / 12 + 1)

date t = year t ++ ", " ++ month t