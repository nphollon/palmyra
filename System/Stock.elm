module System.Stock where

import List as L
import Dict as D
import Maybe as M
import String as S

type alias StockRepo = D.Dict Id Float
type alias Id = String

stocksInfo : StockRepo -> List (Id, String)
stocksInfo = D.map (\k v -> k ++ " : " ++ format v) >> D.toList

format : Float -> String
format x =
  let
    totalCents = round (x * 100)
    dollars = totalCents // 100 |> toString
    cents = totalCents `rem` 100 |> abs |> toString |> S.pad 2 '0'
  in dollars ++ "." ++ cents

depositById : Float -> Id -> StockRepo -> StockRepo
depositById dx id ss =
  case (D.get id ss) of
    Nothing -> ss
    Just x -> D.insert id (max 0 (dx + x)) ss

withdrawById : Float -> Id -> StockRepo -> (Float, StockRepo)
withdrawById dx id ss =
  case (D.get id ss) of
    Nothing -> (0, ss)
    Just x ->
      let (dx', x') = withdraw dx x
      in (dx', D.insert id x' ss)

withdraw : Float -> Float -> (Float, Float)
withdraw dx x =
  if x > dx 
  then (dx, x - dx)
  else (x, 0)
