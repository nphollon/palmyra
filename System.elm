module System where

import System.Stock (Id)
import System.Flow (Flow)
import System.Stock as SS
import System.Flow as SF

import Array as A
import Dict as D
import List as L
import Maybe as M

type alias System = {
    ply : Int,
    stocks : D.Dict Id Float,
    flows : D.Dict Id Flow,
    rules : D.Dict Id Rule
  }

type alias SystemParams = { 
    stocks : D.Dict Id Float,
    flows : D.Dict Id Flow,
    rules : D.Dict Id Rule
  }

type alias Rule = {
    target : Id,
    dependsOn : Id,
    rule : Float -> Maybe Float
  }

new : SystemParams -> System
new sys = { sys | ply = 0 }

getInfo : System -> (List (Id, String), List (Id, Id))
getInfo sys = (SS.stocksInfo sys.stocks, SF.flowsInfo sys.flows)

update : Int -> System -> System
update plyLimit sys =
  if | sys.ply < plyLimit -> evolve sys
     | otherwise -> sys

evolve : System -> System
evolve { ply, stocks, flows, rules } =
  let
    newFlows = D.foldr (always <| applyRule stocks) flows rules
    newStocks = D.foldr (always sourceToSink) stocks newFlows
  in {
    ply = ply + 1,
    stocks = newStocks,
    flows = newFlows,
    rules = rules
  }

applyRule : D.Dict Id Float -> Rule -> D.Dict Id Flow -> D.Dict Id Flow
applyRule stocks rule flows =
  case D.get rule.dependsOn stocks of
    Nothing -> flows
    Just x -> case (rule.rule x) of
      Nothing -> flows
      Just newX -> D.update rule.target (SF.setRate newX |> M.map) flows

sourceToSink : Flow -> D.Dict Id Float -> D.Dict Id Float
sourceToSink flow ss =
  case flow of
    SF.Growth id r ->
      case D.get id ss of
        Just v -> SS.depositById (r * v) id ss
        Nothing -> ss
    SF.Decay id r v0 ->
      case D.get id ss of
        Just v -> SS.withdrawById (r * (v - v0)) id ss |> snd
        Nothing -> ss
    SF.Constant id r -> SS.depositById r id ss
