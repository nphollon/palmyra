module System.Stock where

import List as L
import Dict as D
import Maybe as M
import String (concat)

type alias Stocks = D.Dict Id Stock

repository : List Stock -> Stocks
repository = 
  let addToRepo s dict = D.insert (name s) s dict
  in L.foldr addToRepo D.empty

stocksIn : Scalar -> Id -> Stocks -> Stocks
stocksIn n id ss =
  let s' = getStock id ss |> stockIn n
  in setStock id s' ss

stocksOut : Scalar -> Id -> Stocks -> (Scalar, Stocks)
stocksOut n id ss =
  let (n', s') = getStock id ss |> stockOut n
  in (n', setStock id s' ss)

stocksInfo = D.map (\id s -> concat [ id, " : ", stockInfo s ])


type Stock = Ground | Charge Id Scalar | Mass Id Scalar | Cap Id Scalar Scalar

stockIn : Scalar -> Stock -> Stock
stockIn dx s = 
  case s of 
    Charge n x -> Charge n (x + dx)
    Mass n x -> Mass n (x + dx)
    Cap n c x -> if x + dx < c then Cap n c (x + dx) else Cap n c c
    Ground -> Ground

stockOut : Scalar -> Stock -> (Scalar, Stock)
stockOut dx s =
  case s of
    Charge n x -> (dx, Charge n (x - dx))
    Mass n x -> if dx < x then (dx, Mass n (x - dx)) else (x, Mass n 0)
    Cap n c x -> if dx < x then (dx, Cap n c (x - dx)) else (x, Cap n c 0)
    Ground -> (dx, Ground)

getStock : Id -> Stocks -> Stock
getStock id ss = D.get id ss |> M.withDefault Ground

setStock : Id -> Stock -> Stocks -> Stocks
setStock = D.insert

stockInfo s = 
  case s of
    Charge n x -> toString x
    Mass n x -> concat [ "(", toString x, ")" ]
    Cap n c x -> concat [ "[", toString x, "]" ]
    Ground -> "ground"

name s =
  case s of
    Charge n _ -> n
    Mass n _ -> n
    Cap n _ _ -> n
    Ground -> "ground"


type alias Scalar = Int
type alias Id = String
