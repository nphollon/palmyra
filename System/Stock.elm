module System.Stock where

import Dict as D
import Maybe as M

type alias Stocks = D.Dict Id Stock

stocksIn : Scalar -> Id -> Stocks -> Stocks
stocksIn n id ss =
  let s' = getStock id ss |> stockIn n
  in setStock id s' ss

stocksOut : Scalar -> Id -> Stocks -> (Scalar, Stocks)
stocksOut n id ss =
  let (n', s') = getStock id ss |> stockOut n
  in (n', setStock id s' ss)

stocksInfo = D.values << D.map (\id s -> id ++ " : " ++ (stockInfo s))


type Stock = Ground | Charge Int

stockIn : Scalar -> Stock -> Stock
stockIn dx s = 
  case s of 
    Charge x -> Charge (x + dx)
    Ground -> Ground 

stockOut : Scalar -> Stock -> (Scalar, Stock)
stockOut dx s =
  case s of
    Charge x -> (dx, Charge (x - dx))
    Ground -> (0, Ground)

getStock : Id -> Stocks -> Stock
getStock id ss = D.get id ss |> M.withDefault Ground

setStock : Id -> Stock -> Stocks -> Stocks
setStock = D.insert

stockInfo s = 
  case s of
    Charge x -> toString x
    Ground -> "ground"


type alias Scalar = Int
type alias Id = String
