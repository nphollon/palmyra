module System where

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

stocksIn : Scalar -> Id -> Stocks -> Stocks
stocksIn n id ss =
  let s' = getStock id ss |> stockIn n
  in setStock id s' ss

stockIn : Scalar -> Stock -> Stock
stockIn dx s = 
  case s of 
    Charge x -> Charge (x + dx)
    Ground -> Ground 

stocksOut : Scalar -> Id -> Stocks -> (Scalar, Stocks)
stocksOut n id ss =
  let (n', s') = getStock id ss |> stockOut n
  in (n', setStock id s' ss)

stockOut : Scalar -> Stock -> (Scalar, Stock)
stockOut dx s =
  case s of
    Charge x -> (dx, Charge (x - dx))
    Ground -> (0, Ground)

getStock : Id -> Stocks -> Stock
getStock id ss = D.get id ss |> M.withDefault Ground

setStock : Id -> Stock -> Stocks -> Stocks
setStock = D.insert

scalar : Stock -> Scalar
scalar s =
  case s of
    Charge x -> x
    Ground -> 0

flowIn : Scalar -> Flow -> Flow
flowIn n (Pipe ns r i o) = Pipe (ns ++ [n]) r i o

flowOut : Flow -> (Scalar, Flow)
flowOut (Pipe (n::ns) r i o) = (n, Pipe ns r i o)

rate (Pipe _ r _ _) = r
endpoints (Pipe _ _ i o) = (i,o)
source (Pipe _ _ i _) = i
sink (Pipe _ _ _ o) = o

type alias Flows = List Flow
type Flow = Pipe (List Scalar) Scalar Id Id
type alias Stocks = D.Dict Id Stock
type Stock = Ground | Charge Int
type alias Scalar = Int
type alias Id = String