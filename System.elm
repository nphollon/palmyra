module System where

import List as L
import Dict as D
import Maybe as M

type alias Id = String

type Flow = Flux (Float -> Float -> Float) Id Id
          | Transfer (Float -> Float -> Float -> (Float,Float)) Id Id Id

type System = Sys {
    stocks : D.Dict Id Float,
    flows : List Flow,
    rates : D.Dict Id Float,
    rules : D.Dict Id (System -> Float)
  }

new : { stocks : List (String, Float), rates : List (String, Float, System -> Float), flows : List Flow } -> System
new { stocks, rates, flows } = 
  let (rates', rules') = L.map (\(a,b,c) -> ((a,b),(a,c))) rates |> L.unzip
  in Sys { stocks = D.fromList stocks, flows = flows, rates = D.fromList rates', rules = D.fromList rules' }

getInfo (Sys s) = (s.stocks, s.rates)

evolve : Int -> System -> System
evolve _ = transportMass >> updateRates

transportMass : System -> System
transportMass sys = flows sys |> L.foldl applyFlow sys

applyFlow : Flow -> System -> System
applyFlow flow sys = case flow of
  Flux f r si -> applyFlux f r si sys
  Transfer f r si sj -> applyTransfer f r si sj sys

applyFlux : (Float -> Float -> Float) -> Id -> Id -> System -> System
applyFlux f v i sys =
  case (getRate v sys, getStock i sys) of
    (Just r, Just x) -> setStock i (f r x) sys
    otherwise -> sys

applyTransfer : (Float -> Float -> Float -> (Float, Float)) -> Id -> Id -> Id -> System -> System
applyTransfer f v i j sys =
  let setStocks (a,b) = setStock i a >> setStock j b
  in case (getRate v sys, getStock i sys, getStock j sys) of
    (Just r, Just x, Just y) -> setStocks (f r x y) sys
    otherwise -> sys

updateRates : System -> System
updateRates sys = setRates (D.map (\_ f -> f sys) (rules sys)) sys

flows : System -> List Flow
flows (Sys s) = s.flows

rules : System -> D.Dict Id (System -> Float)
rules (Sys s) = s.rules

getStock : Id -> System -> Maybe Float
getStock i (Sys s) = D.get i s.stocks

setStock : Id -> Float -> System -> System
setStock i x (Sys s) = Sys { s | stocks <- D.insert i x s.stocks }

getRate : Id -> System -> Maybe Float
getRate i (Sys s) = D.get i s.rates

setRates : D.Dict Id Float -> System -> System
setRates rs (Sys s) = Sys { s | rates <- rs }

transform1 : Id -> (Float -> Float) -> System -> Float
transform1 i f sys = case (view i sys) of
  Just x -> f x
  Nothing -> 0

transform2 : Id -> Id -> (Float -> Float -> Float) -> System -> Float
transform2 i j f sys = case (view i sys, view j sys) of
  (Just x, Just y) -> f x y
  otherwise -> 0

view : Id -> System -> Maybe Float
view i sys = M.oneOf [ getStock i sys, getRate i sys ]