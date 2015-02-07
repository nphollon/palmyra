module System.Stock where

import Dict as D
import Maybe as M
import String (concat)

type alias Stocks = D.Dict Id Stock

stocksIn : Scalar -> Id -> Stocks -> Stocks
stocksIn n id ss =
  let s' = getStock id ss |> stockIn n
  in setStock id s' ss

stocksOut : Scalar -> Id -> Stocks -> (Scalar, Stocks)
stocksOut n id ss =
  let (n', s') = getStock id ss |> stockOut n
  in (n', setStock id s' ss)

stocksInfo = D.map (\id s -> concat [ id, " : ", stockInfo s ])


type Stock = Ground | Charge Scalar | Mass Scalar | Cap Scalar Scalar

stockIn : Scalar -> Stock -> Stock
stockIn dx s = 
  case s of 
    Charge x -> Charge (x + dx)
    Mass x -> Mass (x + dx)
    Cap c x -> if x + dx < c then Cap c (x + dx) else Cap c c
    Ground -> Ground

stockOut : Scalar -> Stock -> (Scalar, Stock)
stockOut dx s =
  case s of
    Charge x -> (dx, Charge (x - dx))
    Mass x -> if dx < x then (dx, Mass (x - dx)) else (x, Mass 0)
    Cap c x -> if dx < x then (dx, Cap c (x - dx)) else (x, Cap c 0)
    Ground -> (dx, Ground)

getStock : Id -> Stocks -> Stock
getStock id ss = D.get id ss |> M.withDefault Ground

setStock : Id -> Stock -> Stocks -> Stocks
setStock = D.insert

stockInfo s = 
  case s of
    Charge x -> toString x
    Mass x -> concat [ "(", toString x, ")" ]
    Cap c x -> concat [ "[", toString x, "]" ]
    Ground -> "ground"


type alias Scalar = Int
type alias Id = String
