module System.Flow where

import System.Stock as SS
import System.Stock (StockRepo, Id)

import List as L
import Dict as D
import Maybe as M

type alias FlowParams a = { a | source:Id, sink:Id, stateId:Id, states:D.Dict Id State }
type alias Flow = FlowParams { rate:Rate, pipeline:List Amount }
type alias State = { flux : Maybe Amount -> Maybe Amount -> Rate , rules : List Rule }
type alias Rule = { trigger: Maybe Amount -> Maybe Amount -> Bool, newStateId: Id }
type alias Id = Int
type alias Amount = Float
type alias Rate = Float

new : FlowParams {} -> Flow
new link = 
  let linkWithRate = { link | rate = 1 }
  in  { linkWithRate | pipeline = [] }

flowsInfo : List Flow -> List (Int, Int)
flowsInfo = L.map (\f -> (f.source, f.sink))

flowIn : Amount -> Flow -> Flow
flowIn n flow = { flow | pipeline <- flow.pipeline ++ [n] }

flowOut : Flow -> (Amount, Flow)
flowOut flow =
  let
    (a, newPipeline) = case flow.pipeline of
      (x :: xs) -> (x, xs)
      [] -> (0, [])
  in (a, { flow | pipeline <- newPipeline })

getRate : Maybe Amount -> Maybe Amount -> Flow -> Rate
getRate i o f =
  let maybeState = D.get f.stateId f.states
  in case maybeState of
    Nothing -> 0
    Just {flux, rules} -> flux i o

transitionState : Maybe Amount -> Maybe Amount -> Flow -> Flow
transitionState i o f =
  let
    currentState = D.get f.stateId f.states
    newId = M.andThen currentState (.rules >> checkTriggers i o)
  in { f | stateId <- newId |> M.withDefault f.stateId }
  
checkTriggers : Maybe Amount -> Maybe Amount -> List Rule -> Maybe Id
checkTriggers i o rs =
  if | rs == [] -> Nothing
     | (.trigger (L.head rs)) i o -> Just (.newStateId (L.head rs))
     | otherwise -> checkTriggers i o (L.tail rs)
