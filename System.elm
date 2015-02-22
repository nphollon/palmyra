module System where

import System.Stock (StockRepo)
import System.Flow (Flow, FlowParams)
import System.Stock as SS
import System.Flow as SF

import Array as A
import Dict as D
import List as L

type alias System = {
    ply:Int,
    stocks:StockRepo,
    flows:List Flow
  }

type alias SystemParams = {
  stocks:StockRepo,
  flows:List (FlowParams {})
}

type alias Id = Int

new : SystemParams -> System
new sys = { ply = 0
          , stocks = sys.stocks
          , flows = L.map SF.new sys.flows
        }

getInfo : System -> (List (Id, String), List (Id, Id))
getInfo sys = (SS.stocksInfo sys.stocks, SF.flowsInfo sys.flows)

update : Id -> System -> System
update plyLimit sys =
  if | sys.ply < plyLimit -> evolve sys
     | otherwise -> sys

evolve : System -> System
evolve { ply, stocks, flows } =
  let (newFlow, newStocks) = L.foldr addFlow ([], stocks) flows
  in { ply=ply + 1, stocks=newStocks, flows=newFlow }

addFlow : Flow -> (List Flow, StockRepo) -> (List Flow, StockRepo)
addFlow f (fs, ss) =
  let (f', ss') = sourceFlowSink (f, ss)
  in (f'::fs, ss')

sourceFlowSink : (Flow, StockRepo) -> (Flow, StockRepo)
sourceFlowSink = sourceToFlow >> flowToSink

sourceToFlow : (Flow, StockRepo) -> (Flow, StockRepo)
sourceToFlow (f, ss) =
  let
    sourceValue = SS.valueById f.source ss
    sinkValue = SS.valueById f.sink ss
    rate = SF.flux f sourceValue sinkValue
    (n, ss') = SS.withdrawById rate f.source ss
  in (SF.flowIn n f, ss')

flowToSink : (Flow, StockRepo) -> (Flow, StockRepo)
flowToSink (f, ss) =
  let (n, f') = SF.flowOut f
  in (f', SS.depositById n f.sink ss)
