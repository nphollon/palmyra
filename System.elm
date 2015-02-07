module System where

import System.Types (..)

import List as L
import Dict as D
import Maybe as M

type alias System = {
    ply:Int,
    stocks:Stocks,
    flows:Flows
  }

new : List (String, Stock) -> List (Int, String, String) -> System
new s f = 
  let
    stocks = D.fromList s
    flows = L.map (\(r, i, o) -> Pipe [] r i o) f
  in { ply=0, stocks=stocks, flows=flows }

getInfo : System -> (List (String, Int), List (String, String))
getInfo sys =
  let
    s = D.map (always scalar) sys.stocks |> D.toList
    f = L.map endpoints sys.flows
  in (s, f)

update plyLimit sys =
  if | sys.ply < plyLimit -> evolve sys
     | otherwise -> sys

evolve : System -> System
evolve { ply, stocks, flows } =
  let (newFlows, newStocks) = L.foldr addFlow ([], stocks) flows
  in { ply=ply + 1, stocks=newStocks, flows=newFlows }

addFlow : Flow -> (Flows, Stocks) -> (Flows, Stocks)
addFlow f (fs, ss) =
  let (f', ss') = sourceFlowSink (f, ss)
  in (f'::fs, ss')

sourceFlowSink : (Flow, Stocks) -> (Flow, Stocks)
sourceFlowSink = sourceToFlow >> flowToSink

sourceToFlow : (Flow, Stocks) -> (Flow, Stocks)
sourceToFlow (f, ss) =
  let (n, ss') = stocksOut (rate f) (source f) ss
  in (flowIn n f, ss')

flowToSink : (Flow, Stocks) -> (Flow, Stocks)
flowToSink (f, ss) =
  let (n, f') = flowOut f
  in (f', stocksIn n (sink f) ss)

