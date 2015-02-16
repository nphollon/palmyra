module System.Flow where

import System.Stock as SS
import System.Stock (StockRepo, Id)

import List as L
import Dict as D
import Maybe as M

type Flow = Pipe (List Amount) Rate Id Id
type alias Id = Int
type alias Amount = Float
type alias Rate = Float

initAll : StockRepo -> List (Rate, Id, Id) -> List Flow
initAll = L.filterMap << init

init : StockRepo -> (Rate, Id, Id) -> M.Maybe Flow
init ss (rate, sourceId, sinkId) =
  if | D.member sourceId ss && D.member sinkId ss -> M.Just (Pipe [] rate sourceId sinkId)
     | otherwise -> M.Nothing


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
  let (n, ss') = SS.stocksOut (rate f) (source f) ss
  in (flowIn n f, ss')

flowToSink : (Flow, StockRepo) -> (Flow, StockRepo)
flowToSink (f, ss) =
  let (n, f') = flowOut f
  in (f', SS.stocksIn n (sink f) ss)

flowIn : Amount -> Flow -> Flow
flowIn n (Pipe ns r i o) = Pipe (ns ++ [n]) r i o

flowOut : Flow -> (Amount, Flow)
flowOut (Pipe (n::ns) r i o) = (n, Pipe ns r i o)

rate (Pipe _ r _ _) = r

endpoints (Pipe _ _ i o) = (i,o)

source (Pipe _ _ i _) = i

sink (Pipe _ _ _ o) = o

flowInfo (Pipe _ _ i o) = (i, o)