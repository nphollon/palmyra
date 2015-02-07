module System where

import System.Stock (..)
import System.Flow (..)

import Dict as D
import List as L

type alias System = {
    ply:Int,
    stocks:Stocks,
    flows:Flows
  }

new : List (String, Stock) -> List (Int, String, String) -> System
new s f = 
  let
    stocks = D.fromList s
    flows = L.map (\(r, i, o) -> Pipe [] r i o) f
  in { ply=0, stocks=stocks, flows=flows }

getInfo sys = (stocksInfo sys.stocks, flowsInfo sys.flows)

update plyLimit sys =
  if | sys.ply < plyLimit -> evolve sys
     | otherwise -> sys

evolve : System -> System
evolve { ply, stocks, flows } =
  let (newFlows, newStocks) = L.foldr addFlow ([], stocks) flows
  in { ply=ply + 1, stocks=newStocks, flows=newFlows }
