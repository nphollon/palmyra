module System.Flow where

import System.Stock as SS
import System.Stock (StockRepo, Id)

import List as L
import Dict as D
import Maybe as M

type alias FlowParams a = { a | source:Id, sink:Id, state:Id, states:D.Dict Id State }
type alias Flow = FlowParams { rate:Rate, pipeline:List Amount }
type alias State = { flux : Amount -> Amount -> Rate , rules : List Rule }
type alias Rule = (Amount -> Amount -> Bool, Id)
type alias Id = Int
type alias Amount = Float
type alias Rate = Float

new : FlowParams {} -> Flow
new link = 
  let linkWithRate = { link | rate = 1 }
  in  { linkWithRate | pipeline = [] }

flowsInfo : List Flow -> List (Int, Int)
flowsInfo = L.map flowInfo

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