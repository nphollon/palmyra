module System.Stock where

import List as L
import Dict as D
import Maybe as M

type alias StockRepo = D.Dict Id Stock
type Stock = Ground String | Mass String Amount
type alias Id = Int
type alias Amount = Float

repository : List Stock -> StockRepo
repository stocks = 
  let addToRepo s (i, dict) = (i + 1, D.insert i s dict)
  in L.foldr addToRepo (1, D.empty) stocks |> snd

stocksIn : Amount -> Id -> StockRepo -> StockRepo
stocksIn n id ss =
  let s' = getStock id ss |> stockIn n
  in setStock id s' ss

stocksOut : Amount -> Id -> StockRepo -> (Amount, StockRepo)
stocksOut n id ss =
  let (n', s') = getStock id ss |> stockOut n
  in (n', setStock id s' ss)

stocksInfo : StockRepo -> List (Id, String)
stocksInfo = D.map (always stockInfo) >> D.toList

stockIn : Amount -> Stock -> Stock
stockIn dx s = 
  case s of 
    Mass n x -> Mass n (x + dx)
    Ground n -> Ground n

stockOut : Amount -> Stock -> (Amount, Stock)
stockOut dx s =
  case s of
    Mass n x -> if x > dx then (dx, Mass n (x - dx)) else (x, Mass n 0)
    Ground n -> (dx, Ground n)

getStock : Id -> StockRepo -> Stock
getStock id ss = D.get id ss |> M.withDefault (Ground "ground")

setStock : Id -> Stock -> StockRepo -> StockRepo
setStock = D.insert

stockInfo s = (name s) ++ " : " ++ (value s)

value s =
  case s of
    Mass _ x -> "(" ++ toString x ++ ")"
    Ground n -> "∞"

name s =
  case s of
    Mass n _ -> n
    Ground n -> n
