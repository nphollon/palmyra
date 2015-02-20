module System.Stock where

import List as L
import Dict as D
import Maybe as M
import String as S


type alias StockRepo = D.Dict Id Stock
type Stock = Ground String | Mass String Amount
type alias Id = Int
type alias Amount = Float


repository : List Stock -> StockRepo
repository stocks = 
  let addToRepo s (i, dict) = (i + 1, D.insert i s dict)
  in L.foldr addToRepo (1, D.empty) stocks |> snd


stocksInfo : StockRepo -> List (Id, String)
stocksInfo = D.map (always stockInfo) >> D.toList

stockInfo : Stock -> String
stockInfo s = 
  case s of 
    Mass n x -> n ++ " : " ++ format x
    Ground n -> n ++ " : ∞"

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
    Mass n x -> Mass n (x + dx)
    Ground n -> Ground n


withdrawById : Amount -> Id -> StockRepo -> (Amount, StockRepo)
withdrawById n id ss =
  let (n', s') = getStock id ss |> withdraw n
  in (n', setStock id s' ss)

withdraw : Amount -> Stock -> (Amount, Stock)
withdraw dx s =
  case s of
    Mass n x -> if x > dx then (dx, Mass n (x - dx)) else (x, Mass n 0)
    Ground n -> (dx, Ground n)


getStock : Id -> StockRepo -> Stock
getStock id ss = D.get id ss |> M.withDefault (Ground "ground")

setStock : Id -> Stock -> StockRepo -> StockRepo
setStock = D.insert


valueById : Id -> StockRepo -> Maybe Amount
valueById id ss = getStock id ss |> value

value : Stock -> Maybe Amount
value s =
  case s of
    Mass _ x -> Just x
    Ground _ -> Nothing