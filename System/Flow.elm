module System.Flow where

import System.Stock as SS
import System.Stock (StockRepo, Id)

import List as L
import Dict as D
import Maybe as M

type Flow = Growth Id Float | Decay Id Float Float | Constant Id Float
type alias State = { flux : Maybe Float -> Maybe Float -> Float , rules : List Rule }
type alias Rule = { trigger: Maybe Float -> Maybe Float -> Bool, newStateId: Id }

flowsInfo : D.Dict Id Flow -> List (Id, Id)
flowsInfo = D.values >> L.map print

print : Flow -> (Id, Id)
print flow = ("","")

setRate : Float -> Flow -> Flow
setRate r flow =
  case flow of
    Growth a _ -> Growth a r
    Decay a _ b -> Decay a r b
    Constant a _ -> Constant a r
