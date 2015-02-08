module System.Stock where

import List as L
import Dict as D
import Maybe as M
import String as S

type alias StockRepo = D.Dict Id Stock

repository : List Stock -> StockRepo
repository stocks = 
  let addToRepo s (i, dict) = (i + 1, D.insert i s dict)
  in L.foldr addToRepo (1, D.empty) stocks |> snd

stocksIn : Scalar -> Id -> StockRepo -> StockRepo
stocksIn n id ss =
  let s' = getStock id ss |> stockIn n
  in setStock id s' ss

stocksOut : Scalar -> Id -> StockRepo -> (Scalar, StockRepo)
stocksOut n id ss =
  let (n', s') = getStock id ss |> stockOut n
  in (n', setStock id s' ss)

findByName : String -> StockRepo -> M.Maybe Id
findByName query ss =
  let matches = D.filter (\id v -> name v == query) ss |> D.keys
  in case (L.length matches) of
    1 -> M.Just (L.head matches)
    _ -> M.Nothing

stocksInfo : StockRepo -> D.Dict Id String
stocksInfo = D.map (always stockInfo)


type Stock = Ground | Charge String Scalar | Mass String Scalar | Cap String Scalar Scalar

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

getStock : Id -> StockRepo -> Stock
getStock id ss = D.get id ss |> M.withDefault Ground

setStock : Id -> Stock -> StockRepo -> StockRepo
setStock = D.insert

stockInfo s = (name s) ++ " : " ++ (value s)

value s =
  case s of
    Charge _ x -> toString x
    Mass n x -> S.concat [ "(", toString x, ")" ]
    Cap n c x -> S.concat [ "[", toString x, "]" ]
    Ground -> "∞"

name s =
  case s of
    Charge n _ -> n
    Mass n _ -> n
    Cap n _ _ -> n
    Ground -> "ground"


type alias Scalar = Int
type alias Id = Int
