module System.Types where

import Dict as D
import List as L
import Maybe as M

type alias Stocks = D.Dict Id Stock
type Stock = Ground | Charge Int

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

stocksInfo : Stocks -> List String
stocksInfo = 
  let stockInfo s = 
    case s of
      Charge x -> toString x
      Ground -> "ground"
  in D.values << D.map (\id s -> id ++ " : " ++ (stockInfo s))

type alias Flows = List Flow
type Flow = Pipe (List Scalar) Scalar Id Id

flowIn : Scalar -> Flow -> Flow
flowIn n (Pipe ns r i o) = Pipe (ns ++ [n]) r i o

flowOut : Flow -> (Scalar, Flow)
flowOut (Pipe (n::ns) r i o) = (n, Pipe ns r i o)

rate (Pipe _ r _ _) = r
endpoints (Pipe _ _ i o) = (i,o)
source (Pipe _ _ i _) = i
sink (Pipe _ _ _ o) = o

flowsInfo =
  let flowInfo (Pipe _ _ i o) = i ++ " >> " ++ o
  in L.map flowInfo

type alias Scalar = Int
type alias Id = String
