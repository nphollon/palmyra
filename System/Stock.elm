module System.Stock where

import List as L
import Dict as D
import Maybe as M
import String as S


type alias StockRepo = D.Dict Id Stock
type Stock = Ground | Mass Amount
type alias Id = String
type alias Amount = Float

stocksInfo : StockRepo -> List (Id, String)
stocksInfo = 
  let
    stockInfo s = case s of 
      Mass x -> format x
      Ground -> "∞"
  in D.map (\k v -> k ++ " : " ++ stockInfo v) >> D.toList

format : Amount -> String
format x =
  let
    totalCents = round (x * 100)
    dollars = totalCents // 100 |> toString
    cents = totalCents `rem` 100 |> abs |> toString |> S.pad 2 '0'
  in dollars ++ "." ++ cents


depositById : Amount -> Id -> StockRepo -> StockRepo
depositById n id ss =
  let s' = getStock id ss |> deposit n
  in setStock id s' ss

deposit : Amount -> Stock -> Stock
deposit dx s = 
  case s of 
    Mass x -> Mass (x + dx)
    Ground -> Ground


withdrawById : Amount -> Id -> StockRepo -> (Amount, StockRepo)
withdrawById n id ss =
  let (n', s') = getStock id ss |> withdraw n
  in (n', setStock id s' ss)

withdraw : Amount -> Stock -> (Amount, Stock)
withdraw dx s =
  case s of
    Mass x -> if x > dx then (dx, Mass (x - dx)) else (x, Mass 0)
    Ground -> (dx, Ground)


getStock : Id -> StockRepo -> Stock
getStock id ss = D.get id ss |> M.withDefault Ground

setStock : Id -> Stock -> StockRepo -> StockRepo
setStock = D.insert


valueById : Id -> StockRepo -> Maybe Amount
valueById id ss = getStock id ss |> value

value : Stock -> Maybe Amount
value s =
  case s of
    Mass x -> Just x
    Ground -> Nothing