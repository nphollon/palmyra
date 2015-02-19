module System.Flow where

import System.Stock as SS
import System.Stock (StockRepo, Id)

import List as L
import Dict as D
import Maybe as M

type alias StockLink a = { a | source:Id, sink:Id }
type alias Flow = StockLink { rate:Rate, pipeline:List Amount }
type alias Id = Int
type alias Amount = Float
type alias Rate = Float

new : StockLink {} -> Flow
new link = 
  let linkWithRate = { link | rate = 1 }
  in  { linkWithRate | pipeline = [] }

addFlow : Flow -> (List Flow, StockRepo) -> (List Flow, StockRepo)
addFlow f (fs, ss) =
  let (f', ss') = sourceFlowSink (f, ss)
  in (f'::fs, ss')

flowsInfo : List Flow -> List (Int, Int)
flowsInfo = L.map flowInfo

sourceFlowSink : (Flow, StockRepo) -> (Flow, StockRepo)
sourceFlowSink = sourceToFlow >> flowToSink

sourceToFlow : (Flow, StockRepo) -> (Flow, StockRepo)
sourceToFlow (f, ss) =
  let (n, ss') = SS.stocksOut f.rate f.source ss
  in (flowIn n f, ss')

flowToSink : (Flow, StockRepo) -> (Flow, StockRepo)
flowToSink (f, ss) =
  let (n, f') = flowOut f
  in (f', SS.stocksIn n f.sink ss)

flowIn : Amount -> Flow -> Flow
flowIn n flow = { flow | pipeline <- flow.pipeline ++ [n] }

flowOut : Flow -> (Amount, Flow)
flowOut flow =
  let
    (a, newPipeline) = case flow.pipeline of
      (x :: xs) -> (x, xs)
      [] -> (0, [])
  in (a, { flow | pipeline <- newPipeline })

flowInfo f = (f.source, f.sink)