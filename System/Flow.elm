module System.Flow where

import System.Stock as SS
import System.Stock (StockRepo, Id)

import List as L
import Dict as D
import Maybe as M

type Flow = Deprecate { source:Id, sink:Id, rate:Rate, stateId:Id, states:D.Dict Id State }
  | Growth Id Float
  | Decay Id Float Float
type alias State = { flux : Maybe Amount -> Maybe Amount -> Rate , rules : List Rule }
type alias Rule = { trigger: Maybe Amount -> Maybe Amount -> Bool, newStateId: Id }
type alias Id = SS.Id
type alias Amount = Float
type alias Rate = Float

flowsInfo : List Flow -> List (Id, Id)
flowsInfo = L.map print

print : Flow -> (Id, Id)
print flow = case flow of
  Deprecate f -> (f.source, f.sink)
  otherwise -> ("","")

getRate : Maybe Amount -> Maybe Amount -> Flow -> Rate
getRate i o flow =
  case flow of
    Deprecate f ->
      let maybeState = D.get f.stateId f.states
      in case maybeState of
        Nothing -> 0
        Just {flux, rules} -> flux i o
    Growth _ r -> r
    Decay _ r _ -> r

transitionState : Maybe Amount -> Maybe Amount -> Flow -> Flow
transitionState i o flow =
  case flow of
    Deprecate f ->
      let
        currentState = D.get f.stateId f.states
        newId = M.andThen currentState (.rules >> checkTriggers i o)
      in Deprecate { f | stateId <- newId |> M.withDefault f.stateId }
    otherwise -> flow
  
checkTriggers : Maybe Amount -> Maybe Amount -> List Rule -> Maybe Id
checkTriggers i o rs =
  if | rs == [] -> Nothing
     | (.trigger (L.head rs)) i o -> Just (.newStateId (L.head rs))
     | otherwise -> checkTriggers i o (L.tail rs)
