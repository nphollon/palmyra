module System.Flow where

import System.Stock (..)

import List as L

type alias Flows = List Flow

addFlow : Flow -> (Flows, Stocks) -> (Flows, Stocks)
addFlow f (fs, ss) =
  let (f', ss') = sourceFlowSink (f, ss)
  in (f'::fs, ss')

flowsInfo = L.map flowInfo


type Flow = Pipe (List Scalar) Scalar Id Id

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

flowIn : Scalar -> Flow -> Flow
flowIn n (Pipe ns r i o) = Pipe (ns ++ [n]) r i o

flowOut : Flow -> (Scalar, Flow)
flowOut (Pipe (n::ns) r i o) = (n, Pipe ns r i o)

rate (Pipe _ r _ _) = r
endpoints (Pipe _ _ i o) = (i,o)
source (Pipe _ _ i _) = i
sink (Pipe _ _ _ o) = o
flowInfo (Pipe _ _ i o) = i ++ " >> " ++ o