module System where

import System.Stock (Stock, Id)
import System.Flow (Flow)
import System.Stock as SS
import System.Flow as SF

import Array as A
import Dict as D
import List as L

type alias System = {
    ply : Int,
    stocks : D.Dict Id Stock,
    flows: D.Dict Id Flow
  }

type alias SystemParams = { 
    stocks : D.Dict Id Stock,
    flows : D.Dict Id Flow
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
evolve { ply, stocks, flows } =
  let newStocks = D.foldr (always sourceToSink) stocks flows
  in {
    ply = ply + 1,
    stocks = newStocks,
    flows = D.map (always <| transitionState newStocks) flows
  }

transitionState : D.Dict Id Stock -> Flow -> Flow
transitionState ss flow =
  case flow of
    SF.Deprecate f ->
      let
        sourceValue = SS.valueById f.source ss
        sinkValue = SS.valueById f.sink ss
      in SF.transitionState sourceValue sinkValue flow
    otherwise -> flow

sourceToSink : Flow -> D.Dict Id Stock -> D.Dict Id Stock
sourceToSink flow ss =
  case flow of
    SF.Deprecate f ->
      let
        sourceValue = SS.valueById f.source ss
        sinkValue = SS.valueById f.sink ss
        rate = SF.getRate sourceValue sinkValue flow
        (n, ss') = SS.withdrawById rate f.source ss
        ss'' = SS.depositById n f.sink ss'
      in ss''
    SF.Growth id r ->
      let sinkValue = SS.valueById id ss
      in case sinkValue of
        Just v -> SS.depositById (r * v) id ss
        Nothing -> ss
    SF.Decay id r v0 ->
      let sourceValue = SS.valueById id ss
      in case sourceValue of
        Just v -> SS.withdrawById (r * (v - v0)) id ss |> snd
        Nothing -> ss
